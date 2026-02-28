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

        // Is this a top-level group header?  Starts with ├ or ╰ without a │ prefix.
        let trimmed = line.trim_start_matches(' ');
        if (trimmed.starts_with('├') || trimmed.starts_with('╰')) && !line.starts_with('│') {
            let segs: Vec<&str> = line.splitn(2, '│').collect();
            let name = strip_box(segs[0]).trim().to_string();
            if !name.is_empty() {
                groups.push(Group { name, rows: Vec::new() });
            }
            continue;
        }

        // Is this a bench row?  Starts with │.
        if !line.starts_with('│') {
            continue;
        }

        let segs: Vec<&str> = line.split('│').collect();
        if segs.len() < 3 {
            continue;
        }

        // First segment: tree chars + bench name + fastest value (we want the name only).
        let first = strip_box(segs[0]);
        let first = first.trim();
        if first.is_empty() {
            continue;
        }

        let name = name_only(first);
        // Column order: [name+fastest | slowest | median | mean | samples | iters]
        let Some(median_ns) = parse_time(segs[2].trim()) else { continue };

        if let Some(g) = groups.last_mut() {
            g.rows.push(Row { name, median_ns });
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

fn render(groups: &[Group]) -> String {
    let mut h = String::new();

    h.push_str(r#"<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Bench Report</title>
<style>
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
body{
  background:#0d0f14;color:#c9d1d9;
  font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Helvetica,Arial,sans-serif;
  font-size:14px;line-height:1.5;
  padding:32px 24px;max-width:900px;margin:0 auto;
}
h1{font-size:22px;font-weight:700;color:#f0f6fc;margin-bottom:4px}
.sub{color:#6e7681;font-size:12px;margin-bottom:32px;font-family:monospace}
.group{
  background:#161b22;border:1px solid #21262d;
  border-radius:10px;padding:18px 20px;margin-bottom:14px;
}
.gname{
  font-family:"SFMono-Regular",Consolas,monospace;
  font-size:13px;font-weight:600;color:#79c0ff;
  margin-bottom:14px;letter-spacing:.02em;
}
.row{
  display:grid;
  grid-template-columns:170px 1fr 100px;
  gap:0 10px;align-items:center;margin-bottom:6px;
}
.bname{
  font-family:"SFMono-Regular",Consolas,monospace;
  font-size:12px;color:#8b949e;
  white-space:nowrap;overflow:hidden;text-overflow:ellipsis;
}
.track{background:#21262d;border-radius:4px;height:24px;overflow:hidden}
.fill{height:100%;border-radius:4px;min-width:3px}
.fill-ref   {background:#4a5568}
.fill-win   {background:linear-gradient(90deg,#196c2e,#2ea043)}
.fill-lose  {background:linear-gradient(90deg,#7a1e1e,#c03b3b)}
.fill-other {background:linear-gradient(90deg,#1a3a6c,#2f81c6)}
.meta{
  font-family:"SFMono-Regular",Consolas,monospace;
  font-size:11px;text-align:right;
}
.mtime{color:#e6edf3;display:block}
.mdelta{font-size:10px;display:block}
.win{color:#3fb950}
.lose{color:#f85149}
</style>
</head>
<body>
<h1>Bench Report</h1>
<div class="sub">fad vs serde_json &middot; cargo bench --bench deser_json</div>
"#);

    for group in groups {
        let rows = &group.rows;
        if rows.is_empty() {
            continue;
        }

        let max_ns = rows.iter().map(|r| r.median_ns).fold(0.0f64, f64::max);
        if max_ns == 0.0 {
            continue;
        }

        // Find the serde_json reference (first bench whose name starts with "serde")
        let ref_ns = rows
            .iter()
            .find(|r| r.name.starts_with("serde"))
            .map(|r| r.median_ns);

        write!(h, r#"<div class="group"><div class="gname">{}</div>"#, esc(&group.name)).unwrap();

        for row in rows {
            let pct = (row.median_ns / max_ns * 100.0).clamp(0.5, 100.0);
            let is_ref = row.name.starts_with("serde");

            let fill_class = if is_ref {
                "fill-ref"
            } else if let Some(ref_ns) = ref_ns {
                if row.median_ns < ref_ns {
                    "fill-win"
                } else {
                    "fill-lose"
                }
            } else {
                "fill-other"
            };

            let delta_html = if is_ref {
                String::new()
            } else if let Some(ref_ns) = ref_ns {
                let ratio = row.median_ns / ref_ns;
                if ratio < 1.0 {
                    let gain = (1.0 - ratio) * 100.0;
                    format!(r#"<span class="mdelta win">+{gain:.0}% faster</span>"#)
                } else {
                    let loss = (ratio - 1.0) * 100.0;
                    format!(r#"<span class="mdelta lose">{loss:.0}% slower</span>"#)
                }
            } else {
                String::new()
            };

            write!(
                h,
                r#"<div class="row">
  <div class="bname">{}</div>
  <div class="track"><div class="fill {}" style="width:{:.1}%"></div></div>
  <div class="meta"><span class="mtime">{}</span>{}</div>
</div>"#,
                esc(&row.name),
                fill_class,
                pct,
                fmt_time(row.median_ns),
                delta_html,
            )
            .unwrap();
        }

        h.push_str("</div>\n");
    }

    h.push_str("</body>\n</html>\n");
    h
}
