//! Parse divan bench output from stdin and generate bench_report/index.html.
//!
//!   cargo bench --bench deser_json 2>/dev/null | cargo run --example bench_report
//!   # or via justfile:
//!   just bench-report

use std::fmt::Write as _;
use std::io::Read as _;

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).expect("failed to read stdin");

    let sections = parse_divan(&input);

    if sections.is_empty() || sections.iter().all(|s| s.groups.is_empty()) {
        eprintln!("no groups parsed — pipe in `cargo bench` output");
        std::process::exit(1);
    }

    let html = render(&sections);
    std::fs::create_dir_all("bench_report").unwrap();
    std::fs::write("bench_report/index.html", &html).unwrap();
    eprintln!("→ bench_report/index.html");
}

// ── Types ──────────────────────────────────────────────────────────────────

struct Section {
    label: String,
    groups: Vec<Group>,
}

struct Group {
    name: String,
    rows: Vec<Row>,
}

struct Row {
    name: String,
    median_ns: f64,
}

// ── Parser ─────────────────────────────────────────────────────────────────

fn parse_divan(s: &str) -> Vec<Section> {
    let mut sections: Vec<Section> = Vec::new();

    for line in s.lines() {
        let line = line.trim_end();
        if line.is_empty() {
            continue;
        }

        // Divan header line: "deser_json   fastest │ slowest │ ..."
        // It has no box-drawing prefix and contains "fastest".
        if !line.starts_with('│') && !line.starts_with('├') && !line.starts_with('╰')
            && line.contains("fastest")
        {
            let label = line.split_whitespace().next().unwrap_or("bench").to_string();
            sections.push(Section { label, groups: Vec::new() });
            continue;
        }

        // Ensure there's a section to add to (handle output with no header line).
        if sections.is_empty() {
            sections.push(Section { label: "bench".to_string(), groups: Vec::new() });
        }

        // Find the indentation prefix (leading │ and spaces) and the content after it.
        let prefix_len = line.find(|c| c != '│' && c != ' ').unwrap_or(line.len());
        let prefix = &line[..prefix_len];
        let content = &line[prefix_len..];

        if content.starts_with('├') || content.starts_with('╰') {
            if prefix.is_empty() {
                // No indentation → top-level group header
                let segs: Vec<&str> = line.splitn(2, '│').collect();
                let name = strip_box(segs[0]).trim().to_string();
                if !name.is_empty() {
                    sections.last_mut().unwrap().groups.push(Group { name, rows: Vec::new() });
                }
            } else {
                // Indented → bench entry. Skip leading empty segment from a │-prefixed line.
                let segs: Vec<&str> = line.split('│').collect();
                let s = if segs.first().map_or(true, |s| s.trim().is_empty()) {
                    &segs[1..]
                } else {
                    &segs[..]
                };
                // s[0]=name+fastest, s[1]=slowest, s[2]=median
                if s.len() < 3 { continue; }
                let name = name_only(strip_box(s[0]).trim());
                let Some(median_ns) = parse_time(s[2].trim()) else { continue };
                if !name.is_empty() {
                    if let Some(g) = sections.last_mut().and_then(|s| s.groups.last_mut()) {
                        g.rows.push(Row { name, median_ns });
                    }
                }
            }
        }
    }

    sections
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
    if name.starts_with("fad") { "#E8A820" }          // amber  — fad
    else if name.starts_with("serde") { "#4A9EFF" }   // blue   — serde_json
    else if name.starts_with("postcard") { "#34D399" } // teal   — postcard_serde
    else { "#4B5563" }                                 // slate  — others
}

/// True if this bench is the "reference" to compare others against.
fn is_reference(name: &str) -> bool {
    name.starts_with("serde") || name.starts_with("postcard")
}

/// Sort key: fad_ns / ref_ns, descending (most-slower-first). Groups without
/// both a fad row and a reference row sort to the end.
fn group_sort_key(group: &Group) -> f64 {
    let fad_ns = group.rows.iter().find(|r| r.name.starts_with("fad")).map(|r| r.median_ns);
    let ref_ns = group.rows.iter().find(|r| is_reference(&r.name)).map(|r| r.median_ns);
    match (fad_ns, ref_ns) {
        (Some(f), Some(r)) if r > 0.0 => f / r,
        _ => 0.0, // no comparison available → sort to end
    }
}

fn render(sections: &[Section]) -> String {
    let mut h = String::new();

    // Build tab list (only sections that have renderable groups)
    let active_sections: Vec<&Section> = sections.iter()
        .filter(|s| s.groups.iter().any(|g| g.rows.iter().any(|r| !r.name.starts_with("facet"))))
        .collect();

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
  --fad:#E8A820;--serde:#4A9EFF;--postcard:#34D399;
  --mono:"JetBrains Mono",monospace;
}
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
body{
  background:var(--bg);color:var(--text);
  font-family:var(--mono);font-size:11px;
  padding:24px 20px;max-width:700px;margin:0 auto;
}
header{
  display:flex;align-items:center;gap:16px;
  margin-bottom:16px;
}
h1{
  font-family:"Barlow Condensed",sans-serif;
  font-size:26px;font-weight:700;letter-spacing:.06em;
  color:var(--bright);text-transform:uppercase;
}
.legend{display:flex;gap:10px;align-items:center;margin-left:auto}
.legend-item{display:flex;align-items:center;gap:4px;font-size:10px;color:var(--text)}
.swatch{width:8px;height:8px;border-radius:1px;flex-shrink:0}
/* tabs */
.tabs{display:flex;gap:2px;margin-bottom:12px;border-bottom:1px solid var(--border);padding-bottom:0}
.tab{
  font-family:"Barlow Condensed",sans-serif;
  font-size:13px;font-weight:700;letter-spacing:.06em;text-transform:uppercase;
  color:var(--dim);background:none;border:none;cursor:pointer;
  padding:6px 14px 7px;border-bottom:2px solid transparent;margin-bottom:-1px;
}
.tab:hover{color:var(--text)}
.tab.active{color:var(--bright);border-bottom-color:var(--bright)}
.panel{display:none}
.panel.active{display:block}
/* groups */
.group{
  background:var(--surface);border:1px solid var(--border);
  border-radius:5px;padding:8px 12px;margin-bottom:6px;
}
.gname{
  font-size:9px;font-weight:600;color:var(--dim);
  text-transform:uppercase;letter-spacing:.1em;
  margin-bottom:6px;
}
.row{
  display:grid;
  grid-template-columns:160px 1fr 62px;
  gap:0 8px;align-items:center;margin-bottom:2px;
}
.row:last-child{margin-bottom:0}
.bname{
  font-size:10px;color:var(--text);
  white-space:nowrap;overflow:hidden;text-overflow:ellipsis;
  display:flex;align-items:center;gap:5px;
}
.ratio{
  font-size:9px;color:var(--dim);flex-shrink:0;
}
.ratio.win{color:#3FB950}
.ratio.lose{color:#F85149}
.track{background:#13181F;height:10px;border-radius:2px;overflow:hidden}
.fill{height:100%;min-width:2px;border-radius:2px}
.mtime{font-size:10px;color:var(--bright);text-align:right}
</style>
</head>
<body>
<header>
  <h1>Bench Report</h1>
  <div class="legend">
    <div class="legend-item"><div class="swatch" style="background:var(--fad)"></div>fad</div>
    <div class="legend-item"><div class="swatch" style="background:var(--serde)"></div>serde_json</div>
    <div class="legend-item"><div class="swatch" style="background:var(--postcard)"></div>postcard</div>
  </div>
</header>
"#);

    // Tabs
    h.push_str(r#"<div class="tabs">"#);
    for (i, section) in active_sections.iter().enumerate() {
        let active = if i == 0 { " active" } else { "" };
        write!(h, r#"<button class="tab{}" onclick="switchTab('{}')">{}</button>"#,
            active, esc(&section.label), esc(&section.label)).unwrap();
    }
    h.push_str("</div>\n");

    // Panels
    for (i, section) in active_sections.iter().enumerate() {
        let active = if i == 0 { " active" } else { "" };
        write!(h, r#"<div class="panel{}" id="panel-{}">"#, active, esc(&section.label)).unwrap();

        // Collect and sort groups: most-slower-fad first
        let mut groups: Vec<&Group> = section.groups.iter()
            .filter(|g| g.rows.iter().any(|r| !r.name.starts_with("facet")))
            .collect();
        groups.sort_by(|a, b| group_sort_key(b).partial_cmp(&group_sort_key(a)).unwrap_or(std::cmp::Ordering::Equal));

        for group in groups {
            let rows: Vec<&Row> = group.rows.iter()
                .filter(|r| !r.name.starts_with("facet"))
                .collect();
            if rows.is_empty() { continue; }

            let max_ns = rows.iter().map(|r| r.median_ns).fold(0.0f64, f64::max);
            if max_ns == 0.0 { continue; }

            let ref_ns = rows.iter().find(|r| is_reference(&r.name)).map(|r| r.median_ns);

            write!(h, r#"<div class="group"><div class="gname">{}</div>"#, esc(&group.name)).unwrap();

            for row in &rows {
                let pct = (row.median_ns / max_ns * 100.0).clamp(0.5, 100.0);
                let color = bar_color(&row.name);

                // Ratio label: for fad rows, show "Xx as fast" relative to ref
                let ratio_html = match ref_ns {
                    Some(ref_ns) if row.name.starts_with("fad") && ref_ns > 0.0 => {
                        let x = ref_ns / row.median_ns; // >1 means fad is faster
                        let cls = if x >= 1.0 { "win" } else { "lose" };
                        format!(r#"<span class="ratio {}">{:.2}x</span>"#, cls, x)
                    }
                    _ => String::new(),
                };

                write!(h,
                    r#"<div class="row"><div class="bname">{}{}</div><div class="track"><div class="fill" style="width:{:.1}%;background:{}"></div></div><span class="mtime">{}</span></div>"#,
                    esc(&row.name), ratio_html, pct, color, fmt_time(row.median_ns),
                ).unwrap();
            }

            h.push_str("</div>\n");
        }

        h.push_str("</div>\n"); // close panel
    }

    h.push_str(r#"<script>
function switchTab(label) {
  document.querySelectorAll('.tab').forEach(t => t.classList.toggle('active', t.textContent === label));
  document.querySelectorAll('.panel').forEach(p => p.classList.toggle('active', p.id === 'panel-' + label));
}
</script>
"#);

    h.push_str("</div>\n</body>\n</html>\n");
    h
}
