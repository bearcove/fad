//! Parse divan bench output from stdin and generate bench_report/index.html.
//!
//!   cargo bench --bench deser_json | cargo run --example bench_report

use std::fmt::Write as _;
use std::io::Read as _;

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).expect("failed to read stdin");

    let groups = parse_divan(&input);

    if groups.is_empty() {
        eprintln!("no groups parsed — pipe in `cargo bench` output");
        std::process::exit(1);
    }

    let html = render(&groups);
    std::fs::create_dir_all("bench_report").unwrap();
    std::fs::write("bench_report/index.html", &html).unwrap();
    eprintln!("→ bench_report/index.html");
}

// ── Types ──────────────────────────────────────────────────────────────────

struct Group {
    name: String,
    rows: Vec<Row>,
}

struct Row {
    name: String,
    median_ns: f64,
}

// ── Parser ─────────────────────────────────────────────────────────────────

fn parse_divan(s: &str) -> Vec<Group> {
    let mut groups: Vec<Group> = Vec::new();

    for line in s.lines() {
        let line = line.trim_end();
        if line.is_empty() {
            continue;
        }

        // Find the indentation prefix (leading │ and spaces) and the content after it.
        let prefix_len = line
            .find(|c| c != '│' && c != ' ')
            .unwrap_or(line.len());
        let prefix = &line[..prefix_len];
        let content = &line[prefix_len..];

        if content.starts_with('├') || content.starts_with('╰') {
            if prefix.is_empty() {
                // No indentation → top-level group header
                let segs: Vec<&str> = line.splitn(2, '│').collect();
                let name = strip_box(segs[0]).trim().to_string();
                if !name.is_empty() {
                    groups.push(Group { name, rows: Vec::new() });
                }
            } else {
                // Indented with │ or spaces → bench entry.
                // Split on │ but skip a leading empty segment when the line
                // itself begins with │ (bench entries in multi-group output).
                let segs: Vec<&str> = line.split('│').collect();
                let s = if segs.first().map_or(true, |s| s.trim().is_empty()) {
                    &segs[1..]
                } else {
                    &segs[..]
                };
                // s[0] = name+fastest, s[1] = slowest, s[2] = median
                if s.len() < 3 {
                    continue;
                }
                let name = name_only(strip_box(s[0]).trim());
                let Some(median_ns) = parse_time(s[2].trim()) else { continue };
                if !name.is_empty() {
                    if let Some(g) = groups.last_mut() {
                        g.rows.push(Row { name, median_ns });
                    }
                }
            }
        }
    }

    groups
}

/// Strip all Unicode box-drawing chars: │ ├ ╰ ─ etc.
fn strip_box(s: &str) -> String {
    s.chars()
        .filter(|&c| {
            !('\u{2500}'..='\u{257F}').contains(&c)
        })
        .collect()
}

/// Extract just the name from "bench_name    77.78 ns" — everything before 2+ spaces.
fn name_only(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut end = s.len();
    let mut i = 0;
    while i + 1 < bytes.len() {
        if bytes[i] == b' ' && bytes[i + 1] == b' ' {
            end = i;
            break;
        }
        i += 1;
    }
    s[..end].trim().to_string()
}

/// Parse "77.78 ns" / "1.076 µs" / "3.5 ms" / "1.2 s" → nanoseconds.
fn parse_time(s: &str) -> Option<f64> {
    let s = s.trim();
    let (num, unit) = s.split_once(' ')?;
    let n: f64 = num.parse().ok()?;
    let mul = match unit {
        "ns" => 1.0,
        "µs" => 1_000.0,
        "ms" => 1_000_000.0,
        "s" => 1_000_000_000.0,
        _ => return None,
    };
    Some(n * mul)
}

fn fmt_time(ns: f64) -> String {
    if ns >= 1_000_000_000.0 {
        format!("{:.2} s", ns / 1_000_000_000.0)
    } else if ns >= 1_000_000.0 {
        format!("{:.2} ms", ns / 1_000_000.0)
    } else if ns >= 1_000.0 {
        format!("{:.2} µs", ns / 1_000.0)
    } else {
        format!("{:.1} ns", ns)
    }
}

fn esc(s: &str) -> String {
    s.replace('&', "&amp;").replace('<', "&lt;").replace('>', "&gt;")
}

// ── HTML ───────────────────────────────────────────────────────────────────

fn bar_color(name: &str) -> &'static str {
    if name.starts_with("fad") { "#E8A820" }       // amber — fad
    else if name.starts_with("serde") { "#4A9EFF" } // blue  — serde_json
    else { "#4B5563" }                               // slate — others
}

fn render(groups: &[Group]) -> String {
    let mut h = String::new();

    h.push_str(r#"<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Bench Report</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;600&family=Barlow+Condensed:wght@700&display=swap" rel="stylesheet">
<style>
:root{
  --bg:#080B10;--surface:#0D1117;--border:#1C2128;
  --text:#8B949E;--bright:#E6EDF3;--dim:#484F58;
  --fad:#E8A820;--serde:#4A9EFF;
  --font-mono:"JetBrains Mono",monospace;
}
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
body{
  background:var(--bg);color:var(--text);
  font-family:var(--font-mono);font-size:11px;
  padding:24px 20px;max-width:1100px;margin:0 auto;
}
header{
  display:flex;align-items:baseline;gap:16px;
  margin-bottom:20px;border-bottom:1px solid var(--border);padding-bottom:12px;
}
h1{
  font-family:"Barlow Condensed",sans-serif;
  font-size:28px;font-weight:700;letter-spacing:.05em;
  color:var(--bright);text-transform:uppercase;
}
.legend{display:flex;gap:12px;align-items:center;margin-left:auto}
.legend-item{display:flex;align-items:center;gap:5px;font-size:10px;color:var(--text)}
.swatch{width:10px;height:10px;border-radius:2px;flex-shrink:0}
.grid{
  display:grid;
  grid-template-columns:repeat(auto-fill,minmax(340px,1fr));
  gap:8px;
}
.group{
  background:var(--surface);border:1px solid var(--border);
  border-radius:6px;padding:10px 12px;
}
.gname{
  font-size:10px;font-weight:600;color:var(--dim);
  text-transform:uppercase;letter-spacing:.08em;
  margin-bottom:8px;
}
.row{
  display:grid;
  grid-template-columns:90px 1fr 72px;
  gap:0 8px;align-items:center;margin-bottom:3px;
}
.row:last-child{margin-bottom:0}
.bname{
  font-size:10px;color:var(--text);
  white-space:nowrap;overflow:hidden;text-overflow:ellipsis;
}
.track{
  background:#13181F;height:12px;
  border-radius:2px;overflow:hidden;
}
.fill{height:100%;min-width:2px;border-radius:2px}
.meta{
  font-size:10px;text-align:right;white-space:nowrap;
  display:flex;flex-direction:column;align-items:flex-end;gap:0;
}
.mtime{color:var(--bright)}
.mdelta{font-size:9px;color:var(--dim)}
.mdelta.win{color:#3FB950}
.mdelta.lose{color:#F85149}
</style>
</head>
<body>
<header>
  <h1>Bench Report</h1>
  <div class="legend">
    <div class="legend-item"><div class="swatch" style="background:var(--fad)"></div>fad</div>
    <div class="legend-item"><div class="swatch" style="background:var(--serde)"></div>serde_json</div>
  </div>
</header>
<div class="grid">
"#);

    for group in groups {
        // Filter out facet_json entries
        let rows: Vec<&Row> = group.rows.iter()
            .filter(|r| !r.name.starts_with("facet"))
            .collect();

        if rows.is_empty() { continue; }

        let max_ns = rows.iter().map(|r| r.median_ns).fold(0.0f64, f64::max);
        if max_ns == 0.0 { continue; }

        let ref_ns = rows.iter()
            .find(|r| r.name.starts_with("serde"))
            .map(|r| r.median_ns);

        write!(h, r#"<div class="group"><div class="gname">{}</div>"#, esc(&group.name)).unwrap();

        for row in &rows {
            let pct = (row.median_ns / max_ns * 100.0).clamp(0.5, 100.0);
            let color = bar_color(&row.name);

            let delta_html = match ref_ns {
                Some(ref_ns) if !row.name.starts_with("serde") => {
                    let ratio = row.median_ns / ref_ns;
                    if ratio < 1.0 {
                        format!(r#"<span class="mdelta win">+{:.0}%</span>"#, (1.0 - ratio) * 100.0)
                    } else {
                        format!(r#"<span class="mdelta lose">-{:.0}%</span>"#, (ratio - 1.0) * 100.0)
                    }
                }
                _ => String::new(),
            };

            write!(h, r#"<div class="row"><div class="bname">{}</div><div class="track"><div class="fill" style="width:{:.1}%;background:{}"></div></div><div class="meta"><span class="mtime">{}</span>{}</div></div>"#,
                esc(&row.name), pct, color, fmt_time(row.median_ns), delta_html,
            ).unwrap();
        }

        h.push_str("</div>\n");
    }

    h.push_str("</div>\n</body>\n</html>\n");
    h
}
