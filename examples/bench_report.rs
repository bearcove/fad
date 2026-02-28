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

    let meta = Meta::collect();
    let html = render(&sections, &meta);
    std::fs::create_dir_all("bench_report").unwrap();
    std::fs::write("bench_report/index.html", &html).unwrap();
    eprintln!("→ bench_report/index.html");
}

// ── Types ──────────────────────────────────────────────────────────────────

struct Meta {
    datetime: String,
    commit: String,
    platform: String,
}

impl Meta {
    fn collect() -> Self {
        let datetime = sh("date", &["+%Y-%m-%d %H:%M %Z"]);
        let commit = sh("git", &["rev-parse", "--short", "HEAD"]);
        let uname = sh("uname", &["-srm"]);
        let cpu = cpu_name();
        let platform = if cpu.is_empty() { uname } else { format!("{uname} · {cpu}") };
        Meta { datetime, commit, platform }
    }
}

fn sh(prog: &str, args: &[&str]) -> String {
    std::process::Command::new(prog)
        .args(args)
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .unwrap_or_default()
        .trim()
        .to_string()
}

fn cpu_name() -> String {
    if let Ok(info) = std::fs::read_to_string("/proc/cpuinfo") {
        for line in info.lines() {
            if line.starts_with("model name") {
                if let Some(val) = line.split(':').nth(1) {
                    return val.trim().to_string();
                }
            }
        }
    }
    sh("sysctl", &["-n", "machdep.cpu.brand_string"])
}

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
        if line.is_empty() { continue; }

        if !line.starts_with('│') && !line.starts_with('├') && !line.starts_with('╰')
            && line.contains("fastest")
        {
            let label = line.split_whitespace().next().unwrap_or("bench").to_string();
            sections.push(Section { label, groups: Vec::new() });
            continue;
        }

        if sections.is_empty() {
            sections.push(Section { label: "bench".to_string(), groups: Vec::new() });
        }

        let prefix_len = line.find(|c| c != '│' && c != ' ').unwrap_or(line.len());
        let prefix = &line[..prefix_len];
        let content = &line[prefix_len..];

        if content.starts_with('├') || content.starts_with('╰') {
            if prefix.is_empty() {
                let segs: Vec<&str> = line.splitn(2, '│').collect();
                let name = strip_box(segs[0]).trim().to_string();
                if !name.is_empty() {
                    sections.last_mut().unwrap().groups.push(Group { name, rows: Vec::new() });
                }
            } else {
                let segs: Vec<&str> = line.split('│').collect();
                let s = if segs.first().map_or(true, |s| s.trim().is_empty()) {
                    &segs[1..]
                } else {
                    &segs[..]
                };
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

fn strip_box(s: &str) -> String {
    s.chars().filter(|&c| !('\u{2500}'..='\u{257F}').contains(&c)).collect()
}

fn name_only(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut end = s.len();
    let mut i = 0;
    while i + 1 < bytes.len() {
        if bytes[i] == b' ' && bytes[i + 1] == b' ' { end = i; break; }
        i += 1;
    }
    s[..end].trim().to_string()
}

fn parse_time(s: &str) -> Option<f64> {
    let (num, unit) = s.trim().split_once(' ')?;
    let n: f64 = num.parse().ok()?;
    let mul = match unit { "ns" => 1.0, "µs" => 1_000.0, "ms" => 1_000_000.0, "s" => 1_000_000_000.0, _ => return None };
    Some(n * mul)
}

fn fmt_time_html(ns: f64) -> String {
    let (num, unit) = if ns >= 1_000_000_000.0 { (format!("{:.2}", ns / 1_000_000_000.0), "s") }
        else if ns >= 1_000_000.0 { (format!("{:.2}", ns / 1_000_000.0), "ms") }
        else if ns >= 1_000.0 { (format!("{:.2}", ns / 1_000.0), "µs") }
        else { (format!("{:.1}", ns), "ns") };
    format!(r#"{}<span class="unit"> {}</span>"#, num, unit)
}

fn esc(s: &str) -> String {
    s.replace('&', "&amp;").replace('<', "&lt;").replace('>', "&gt;")
}

fn display_name(name: &str) -> String {
    if name == "postcard_serde" { return "serde".to_string(); }
    name.replace("serde_json", "serde")
}

// ── HTML ───────────────────────────────────────────────────────────────────

fn is_reference(name: &str) -> bool {
    name.starts_with("serde") || name.starts_with("postcard")
}

fn group_sort_key(group: &Group) -> f64 {
    let fad_ns = group.rows.iter().filter(|r| r.name.starts_with("fad")).map(|r| r.median_ns).fold(f64::INFINITY, f64::min);
    let ref_ns = group.rows.iter().find(|r| is_reference(&r.name)).map(|r| r.median_ns);
    match ref_ns {
        Some(r) if r > 0.0 && fad_ns.is_finite() => fad_ns / r,
        _ => 0.0,
    }
}

/// Compute delta bar fill: returns (fill_pct of one half, fad_wins).
/// ratio = ref_ns / fad_ns; >1 means fad is faster.
fn delta_fill(ratio: f64) -> (f64, bool) {
    let fad_wins = ratio >= 1.0;
    let r = if fad_wins { ratio } else { 1.0 / ratio };
    let fill = ((r - 1.0) / 3.0 * 50.0_f64).min(50.0);
    (fill, fad_wins)
}

fn render(sections: &[Section], meta: &Meta) -> String {
    let mut h = String::new();

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
<link href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=Plus+Jakarta+Sans:wght@600;700&display=swap" rel="stylesheet">
<style>
:root{
  --bg:#070A0F;--surface:#0C1118;--border:rgba(255,255,255,0.07);
  --track:#1A2535;
  --text:#AAB8C8;--bright:#E4EEF8;--dim:#6A7A8A;
  --fad:#D4A030;--ref:#4A8ED4;
  --mono:"IBM Plex Mono",monospace;--sans:"Plus Jakarta Sans",sans-serif;
}
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
body{
  background:var(--bg);color:var(--text);
  font-family:var(--mono);font-size:13px;
  padding:24px 20px;max-width:960px;margin:0 auto;
}
header{
  display:flex;align-items:flex-start;gap:16px;
  margin-bottom:20px;
}
.title-block{flex:1}
h1{
  font-family:var(--sans);
  font-size:20px;font-weight:700;letter-spacing:-.02em;
  color:var(--bright);
}
.meta{font-size:13px;color:var(--bright);margin-top:5px;letter-spacing:.01em;font-family:var(--mono)}
.legend{display:flex;gap:12px;align-items:center;padding-top:6px}
.legend-item{display:flex;align-items:center;gap:5px;font-size:12px;color:var(--text)}
.swatch{width:8px;height:8px;border-radius:1px;flex-shrink:0}
/* tabs */
.tabs{display:flex;gap:0;margin-bottom:16px;border-bottom:1px solid rgba(255,255,255,0.07)}
.tab{
  font-family:var(--sans);
  font-size:13px;font-weight:700;letter-spacing:-.01em;
  color:var(--dim);background:none;border:none;cursor:pointer;
  padding:8px 16px 9px;border-bottom:2px solid transparent;margin-bottom:-1px;
}
.tab:hover{color:var(--text)}
.tab.active{color:var(--bright);border-bottom-color:var(--fad)}
.panel{display:none}
.panel.active{display:block}
/* summary */
.summary{
  font-family:var(--sans);font-size:14px;font-weight:700;
  text-align:center;margin-bottom:16px;
  color:var(--text);
}
.summary .fad-n{color:#3DB858}
.summary .ref-n{color:var(--ref)}
.summary .pipe{color:var(--dim);margin:0 10px}
/* bench rows */
.bench-row{
  display:grid;
  grid-template-columns:180px 64px 1fr 64px;
  gap:0 10px;align-items:center;margin-bottom:10px;
}
.bench-row:last-child{margin-bottom:0}
.group{margin-bottom:4px}
.bname{
  font-size:12px;font-weight:600;color:#A0B8CC;
  letter-spacing:.04em;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;
}
.t-fad{
  font-size:12px;font-weight:500;color:var(--bright);
  text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap;
}
.t-ref{
  font-size:12px;font-weight:500;color:var(--bright);
  font-variant-numeric:tabular-nums;white-space:nowrap;
}
.unit{font-size:11px;color:var(--text);font-weight:400}
/* bar cell: ratio label sits above the track */
.bar-cell{display:flex;flex-direction:column;gap:3px}
.ratio-above{
  font-family:var(--mono);font-size:11px;font-weight:600;
  font-variant-numeric:tabular-nums;text-align:center;line-height:1;
}
.ratio-above.win{color:#3DB858}
.ratio-above.lose{color:#C05050}
/* the track */
.delta-track{
  position:relative;height:8px;
  background:var(--track);border-radius:4px;
}
.delta-track::after{
  content:'';position:absolute;
  left:calc(50% - 0.5px);top:0;bottom:0;width:1px;
  background:rgba(255,255,255,0.2);z-index:1;pointer-events:none;
}
.delta-fill{position:absolute;top:0;bottom:0}
.delta-fill.fad-side{right:50%;border-radius:4px 0 0 4px;background:var(--fad)}
.delta-fill.ref-side{left:50%;border-radius:0 4px 4px 0;background:var(--ref)}
</style>
</head>
<body>
<header>
  <div class="title-block">
    <h1>Bench Report</h1>
    <div class="meta">"#);

    write!(h, "{} · {} · {}", esc(&meta.datetime), esc(&meta.commit), esc(&meta.platform)).unwrap();

    h.push_str(r#"</div>
  </div>
  <div class="legend">
    <div class="legend-item"><div class="swatch" style="background:var(--fad)"></div>fad</div>
    <div class="legend-item"><div class="swatch" style="background:var(--ref)"></div>serde</div>
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

        let mut groups: Vec<&Group> = section.groups.iter()
            .filter(|g| g.rows.iter().any(|r| !r.name.starts_with("facet")))
            .collect();
        groups.sort_by(|a, b| group_sort_key(b).partial_cmp(&group_sort_key(a)).unwrap_or(std::cmp::Ordering::Equal));

        // Win summary
        let comparable_total = groups.iter().filter(|g| {
            g.rows.iter().any(|r| r.name.starts_with("fad")) &&
            g.rows.iter().any(|r| is_reference(&r.name))
        }).count();
        let fad_wins = groups.iter().filter(|g| {
            let best_fad = g.rows.iter()
                .filter(|r| r.name.starts_with("fad"))
                .map(|r| r.median_ns)
                .fold(f64::INFINITY, f64::min);
            let Some(ref_row) = g.rows.iter().find(|r| is_reference(&r.name)) else { return false };
            ref_row.median_ns >= best_fad
        }).count();
        if comparable_total > 0 {
            let ref_name = groups.iter().find_map(|g| g.rows.iter().find(|r| is_reference(&r.name)))
                .map(|r| display_name(&r.name))
                .unwrap_or_else(|| "serde".to_string());
            let serde_wins = comparable_total - fad_wins;
            write!(h,
                r#"<div class="summary">fad wins <span class="fad-n">{fad_wins}</span><span class="pipe">|</span>{ref_name} wins <span class="ref-n">{serde_wins}</span></div>"#,
            ).unwrap();
        }

        for group in &groups {
            let rows: Vec<&Row> = group.rows.iter()
                .filter(|r| !r.name.starts_with("facet"))
                .collect();
            if rows.is_empty() { continue; }

            // Prefer fad_from_str over fad_from_bytes; hide fad_from_bytes if others exist
            let all_fad: Vec<&Row> = rows.iter()
                .filter(|r| r.name.starts_with("fad"))
                .copied()
                .collect();
            let fad_rows: Vec<&Row> = {
                let without_bytes: Vec<&Row> = all_fad.iter()
                    .copied()
                    .filter(|r| r.name != "fad_from_bytes")
                    .collect();
                if without_bytes.is_empty() { all_fad } else { without_bytes }
            };
            let ref_row = rows.iter().find(|r| is_reference(&r.name)).copied();
            if fad_rows.is_empty() { continue; }

            for fad_row in &fad_rows {
                let (bar_fill, ratio_above) = match ref_row {
                    Some(rr) => {
                        let ratio = rr.median_ns / fad_row.median_ns;
                        let (fill, fad_wins) = delta_fill(ratio);
                        let fill_class = if fad_wins { "delta-fill fad-side" } else { "delta-fill ref-side" };
                        let ratio_cls = if fad_wins { "win" } else { "lose" };
                        (
                            format!(r#"<div class="{fill_class}" style="width:{fill:.1}%"></div>"#),
                            format!(r#"<span class="ratio-above {ratio_cls}">{:.2}×</span>"#, ratio),
                        )
                    }
                    None => (
                        r#"<div class="delta-fill fad-side" style="width:40%"></div>"#.to_string(),
                        String::new(),
                    ),
                };

                let ref_time = ref_row.map(|r| fmt_time_html(r.median_ns)).unwrap_or_else(|| "—".to_string());

                write!(h,
                    r#"<div class="bench-row"><span class="bname">{}</span><span class="t-fad">{}</span><div class="bar-cell">{ratio_above}<div class="delta-track">{bar_fill}</div></div><span class="t-ref">{ref_time}</span></div>"#,
                    esc(&group.name), fmt_time_html(fad_row.median_ns),
                ).unwrap();
            }
        }

        h.push_str("</div>\n");
    }

    h.push_str(r#"<script>
function switchTab(label) {
  document.querySelectorAll('.tab').forEach(t => t.classList.toggle('active', t.textContent === label));
  document.querySelectorAll('.panel').forEach(p => p.classList.toggle('active', p.id === 'panel-' + label));
}
</script>
</body>
</html>
"#);

    h
}
