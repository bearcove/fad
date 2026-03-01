//! Parse NDJSON bench output from stdin and generate bench_report/{index.html,results.json,results.md}.
//!
//! Reads line-by-line, showing live progress on stderr.
//!
//!   cargo bench 2>/dev/null | cargo run --example bench_report

use std::fmt::Write as _;
use std::io::BufRead as _;

fn main() {
    let stdin = std::io::stdin();
    let mut lines: Vec<String> = Vec::new();

    // Progress state
    let mut total_expected: usize = 0;
    let mut completed: usize = 0;

    for line in stdin.lock().lines() {
        let line = match line {
            Ok(l) => l,
            Err(_) => break,
        };
        let trimmed = line.trim();
        if trimmed.is_empty() || !trimmed.starts_with('{') {
            continue;
        }

        // Parse for progress display
        if let Ok(v) = serde_json::from_str::<serde_json::Value>(trimmed) {
            match v["type"].as_str() {
                Some("suite") => {
                    let n = v["total"].as_u64().unwrap_or(0) as usize;
                    total_expected += n;
                    eprint!("\r\x1b[2K  [{completed}/{total_expected}] waiting...");
                }
                Some("start") => {
                    if let Some(name) = v["name"].as_str() {
                        eprint!("\r\x1b[2K  [{completed}/{total_expected}] {name}");
                    }
                }
                Some("result") => {
                    completed += 1;
                    if let (Some(name), Some(median)) =
                        (v["name"].as_str(), v["median_ns"].as_f64())
                    {
                        eprint!(
                            "\r\x1b[2K  [{completed}/{total_expected}] {name} ... {}",
                            fmt_time(median)
                        );
                    }
                }
                _ => {}
            }
        }

        lines.push(line);
    }

    // Clear progress line
    eprint!("\r\x1b[2K");

    let input = lines.join("\n");
    let sections = parse_ndjson(&input);

    if sections.is_empty() || sections.iter().all(|s| s.groups.is_empty()) {
        eprintln!("no groups parsed — pipe in `cargo bench` NDJSON output");
        std::process::exit(1);
    }

    let meta = Meta::collect();
    let html = render(&sections, &meta);
    let json = render_json(&sections, &meta);
    let md = render_markdown(&sections, &meta);
    std::fs::create_dir_all("bench_report").unwrap();
    std::fs::write("bench_report/index.html", &html).unwrap();
    std::fs::write("bench_report/results.json", &json).unwrap();
    std::fs::write("bench_report/results.md", &md).unwrap();
    eprintln!("→ bench_report/index.html  ({completed} benchmarks)");
    eprintln!("→ bench_report/results.json");
    eprintln!("→ bench_report/results.md");
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

// ── Parser (NDJSON) ───────────────────────────────────────────────────────

fn parse_ndjson(input: &str) -> Vec<Section> {
    let mut entries: Vec<(Vec<String>, f64)> = Vec::new();

    for line in input.lines() {
        let line = line.trim();
        if line.is_empty() || !line.starts_with('{') {
            continue;
        }
        let v: serde_json::Value = match serde_json::from_str(line) {
            Ok(v) => v,
            Err(_) => continue,
        };
        // Only process result events (skip suite/start)
        match v["type"].as_str() {
            Some("suite") | Some("start") => continue,
            _ => {}
        }
        let Some(name) = v["name"].as_str() else { continue };
        let Some(median_ns) = v["median_ns"].as_f64() else { continue };

        let parts: Vec<String> = name.split('/').map(String::from).collect();
        entries.push((parts, median_ns));
    }

    organize_entries(&entries)
}

/// Reorganize flat entries into Section/Group/Row hierarchy.
///
/// - 3-component paths `[type, format, fn]` from the `bench!` macro:
///   section = "{format} {direction}", group = type, row = fn
/// - 2-component paths `[group, fn]` from manual bench groups:
///   infer format from group name prefix, same routing
/// - 1-component paths `[fn]` from flat benchmarks:
///   section = "json deser", group = first component, row = fn
fn organize_entries(entries: &[(Vec<String>, f64)]) -> Vec<Section> {
    let mut sections: Vec<Section> = Vec::new();

    for (path, median_ns) in entries {
        let (section_label, group_name, row_name) = match path.len() {
            3 => {
                let direction = if path[2].ends_with("_ser") { "ser" } else { "deser" };
                (format!("{} {direction}", path[1]), path[0].clone(), path[2].clone())
            }
            2 => {
                // Manual groups: infer format from group name prefix
                let (format, clean_name) = if path[0].starts_with("postcard_") {
                    ("postcard", path[0]["postcard_".len()..].to_string())
                } else {
                    ("json", path[0].clone())
                };
                let direction = if path[1].ends_with("_ser") { "ser" } else { "deser" };
                (format!("{format} {direction}"), clean_name, path[1].clone())
            }
            1 => {
                ("json deser".to_string(), path[0].clone(), path[0].clone())
            }
            _ => continue,
        };

        // Find or create section
        let section = match sections.iter_mut().find(|s| s.label == section_label) {
            Some(s) => s,
            None => {
                sections.push(Section { label: section_label, groups: Vec::new() });
                sections.last_mut().unwrap()
            }
        };

        // Find or create group within section
        let group = match section.groups.iter_mut().find(|g| g.name == group_name) {
            Some(g) => g,
            None => {
                section.groups.push(Group { name: group_name, rows: Vec::new() });
                section.groups.last_mut().unwrap()
            }
        };

        group.rows.push(Row { name: row_name, median_ns: *median_ns });
    }

    sections
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

/// Strip _deser/_ser suffix for display, then apply legacy renames.
fn display_name(name: &str) -> String {
    let base = name.strip_suffix("_deser")
        .or_else(|| name.strip_suffix("_ser"))
        .unwrap_or(name);
    if base == "postcard_serde" { return "serde".to_string(); }
    base.replace("serde_json", "serde").to_string()
}

// ── JSON ───────────────────────────────────────────────────────────────────

fn render_json(sections: &[Section], meta: &Meta) -> String {
    let mut j = String::new();
    j.push_str("{\n");
    write!(j, r#"  "datetime": "{}","#, meta.datetime).unwrap();
    j.push('\n');
    write!(j, r#"  "commit": "{}","#, meta.commit).unwrap();
    j.push('\n');
    write!(j, r#"  "platform": "{}","#, meta.platform.replace('"', r#"\""#)).unwrap();
    j.push('\n');
    j.push_str(r#"  "sections": ["#);
    j.push('\n');
    for (si, section) in sections.iter().enumerate() {
        j.push_str("    {\n");
        write!(j, r#"      "label": "{}","#, section.label).unwrap();
        j.push('\n');
        j.push_str(r#"      "groups": ["#);
        j.push('\n');
        for (gi, group) in section.groups.iter().enumerate() {
            j.push_str("        {\n");
            write!(j, r#"          "name": "{}","#, group.name).unwrap();
            j.push('\n');
            j.push_str(r#"          "rows": ["#);
            j.push('\n');
            for (ri, row) in group.rows.iter().enumerate() {
                let comma = if ri + 1 < group.rows.len() { "," } else { "" };
                write!(j, r#"            {{ "name": "{}", "median_ns": {:.1} }}{comma}"#, row.name, row.median_ns).unwrap();
                j.push('\n');
            }
            j.push_str("          ]\n");
            let comma = if gi + 1 < section.groups.len() { "," } else { "" };
            write!(j, "        }}{comma}").unwrap();
            j.push('\n');
        }
        j.push_str("      ]\n");
        let comma = if si + 1 < sections.len() { "," } else { "" };
        write!(j, "    }}{comma}").unwrap();
        j.push('\n');
    }
    j.push_str("  ]\n");
    j.push_str("}\n");
    j
}

// ── Markdown ───────────────────────────────────────────────────────────────

fn fmt_time(ns: f64) -> String {
    if ns >= 1_000_000_000.0 { format!("{:.2}s", ns / 1_000_000_000.0) }
    else if ns >= 1_000_000.0 { format!("{:.2}ms", ns / 1_000_000.0) }
    else if ns >= 1_000.0 { format!("{:.2}µs", ns / 1_000.0) }
    else { format!("{:.1}ns", ns) }
}

fn render_markdown(sections: &[Section], meta: &Meta) -> String {
    let mut m = String::new();
    writeln!(m, "# Bench Report").unwrap();
    writeln!(m).unwrap();
    writeln!(m, "> {} · {} · {}", meta.datetime, meta.commit, meta.platform).unwrap();
    writeln!(m).unwrap();

    for section in sections {
        // Collect all (group, fad_row, ref_row) pairs that have both fad and reference
        let mut comparisons: Vec<(&str, f64, f64, &str)> = Vec::new();
        for group in &section.groups {
            let fad = group.rows.iter()
                .filter(|r| r.name.starts_with("fad"))
                .min_by(|a, b| a.median_ns.partial_cmp(&b.median_ns).unwrap());
            let reference = group.rows.iter()
                .find(|r| is_reference(&r.name));
            if let (Some(f), Some(r)) = (fad, reference) {
                comparisons.push((&group.name, f.median_ns, r.median_ns, &r.name));
            }
        }

        if comparisons.is_empty() { continue; }

        // Sort by ratio (fad/ref): serde wins first (ratio > 1), then fad wins (ratio < 1)
        comparisons.sort_by(|a, b| {
            let ra = a.1 / a.2;
            let rb = b.1 / b.2;
            rb.partial_cmp(&ra).unwrap_or(std::cmp::Ordering::Equal)
        });

        let ref_label = display_name(comparisons[0].3);

        writeln!(m, "## {}", section.label).unwrap();
        writeln!(m).unwrap();
        writeln!(m, "| Benchmark | fad | {ref_label} | ratio |").unwrap();
        writeln!(m, "|-----------|-----|-------|-------|").unwrap();

        for (name, fad_ns, ref_ns, _) in &comparisons {
            let ratio = ref_ns / fad_ns;
            let indicator = if ratio > 1.05 {
                format!("**{ratio:.2}x** faster")
            } else if ratio < 0.95 {
                format!("**{:.2}x** slower", 1.0 / ratio)
            } else {
                "~tie".to_string()
            };
            writeln!(m, "| {name} | {} | {} | {indicator} |",
                fmt_time(*fad_ns), fmt_time(*ref_ns)).unwrap();
        }

        writeln!(m).unwrap();

        let fad_wins = comparisons.iter().filter(|(_, f, r, _)| f < r).count();
        let ref_wins = comparisons.iter().filter(|(_, f, r, _)| f > r).count();
        let ties = comparisons.len() - fad_wins - ref_wins;
        write!(m, "**fad wins {fad_wins}").unwrap();
        if ties > 0 { write!(m, ", ties {ties}").unwrap(); }
        writeln!(m, ", {ref_label} wins {ref_wins}**").unwrap();
        writeln!(m).unwrap();
    }

    m
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

    let active_sections: Vec<&Section> = sections.iter().collect();

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
/* tabs */
.tabs{display:flex;gap:0;margin-bottom:16px;border-bottom:1px solid rgba(255,255,255,0.07);flex-wrap:wrap}
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
/* bench rows — 5 cols: name | ratio | fad-time | bar | serde-time */
.bench-row{
  display:grid;
  grid-template-columns:180px 44px 64px 1fr 64px;
  gap:0 10px;align-items:center;margin-bottom:10px;
}
.bench-row:last-child{margin-bottom:0}
.bname{
  font-size:12px;font-weight:600;color:#A0B8CC;
  letter-spacing:.04em;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;
}
.ratio-col{
  font-size:11px;font-weight:600;
  font-variant-numeric:tabular-nums;
  text-align:right;white-space:nowrap;
}
.ratio-col.win{color:#3DB858}
.ratio-col.lose{color:#C05050}
.t-fad{
  font-size:12px;font-weight:500;color:var(--bright);
  text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap;
}
.t-ref{
  font-size:12px;font-weight:500;color:var(--bright);
  font-variant-numeric:tabular-nums;white-space:nowrap;
}
.unit{font-size:11px;color:var(--text);font-weight:400}
/* bar cell */
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
/* summary row — lives in the bar column */
.summary-cell{
  font-family:var(--sans);font-size:13px;font-weight:700;
  text-align:center;padding-bottom:6px;color:var(--text);
}
.summary-cell .fad-n{color:#3DB858}
.summary-cell .ref-n{color:var(--ref)}
.summary-cell .pipe{color:var(--dim);margin:0 10px}
/* footer */
footer{
  margin-top:24px;padding-top:12px;
  border-top:1px solid var(--border);
  font-size:12px;color:var(--dim);
}
</style>
</head>
<body>
"#);

    // Tabs
    h.push_str(r#"<div class="tabs">"#);
    for (i, section) in active_sections.iter().enumerate() {
        let active = if i == 0 { " active" } else { "" };
        let tab_id = tab_id(&section.label);
        write!(h, r#"<button class="tab{}" onclick="switchTab('{}')">{}</button>"#,
            active, tab_id, esc(&section.label)).unwrap();
    }
    h.push_str("</div>\n");

    // Panels
    for (i, section) in active_sections.iter().enumerate() {
        let active = if i == 0 { " active" } else { "" };
        let tab_id = tab_id(&section.label);
        write!(h, r#"<div class="panel{}" id="panel-{}">"#, active, tab_id).unwrap();

        let mut groups: Vec<&Group> = section.groups.iter().collect();
        groups.sort_by(|a, b| group_sort_key(b).partial_cmp(&group_sort_key(a)).unwrap_or(std::cmp::Ordering::Equal));

        // Win summary row
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
                r#"<div class="bench-row"><span></span><span></span><span></span><div class="summary-cell">fad wins <span class="fad-n">{fad_wins}</span><span class="pipe">|</span>{ref_name} wins <span class="ref-n">{serde_wins}</span></div><span></span></div>"#,
            ).unwrap();
        }

        for group in &groups {
            let rows: Vec<&Row> = group.rows.iter().collect();
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
                let (bar_fill, ratio_html) = match ref_row {
                    Some(rr) => {
                        let ratio = rr.median_ns / fad_row.median_ns;
                        let (fill, fad_wins) = delta_fill(ratio);
                        let fill_class = if fad_wins { "delta-fill fad-side" } else { "delta-fill ref-side" };
                        let ratio_cls = if fad_wins { "win" } else { "lose" };
                        (
                            format!(r#"<div class="{fill_class}" style="width:{fill:.1}%"></div>"#),
                            format!(r#"<span class="ratio-col {ratio_cls}">{:.2}×</span>"#, ratio),
                        )
                    }
                    None => (
                        r#"<div class="delta-fill fad-side" style="width:40%"></div>"#.to_string(),
                        String::new(),
                    ),
                };

                let ref_time = ref_row.map(|r| fmt_time_html(r.median_ns)).unwrap_or_else(|| "—".to_string());

                write!(h,
                    r#"<div class="bench-row"><span class="bname">{}</span>{ratio_html}<span class="t-fad">{}</span><div class="delta-track">{bar_fill}</div><span class="t-ref">{ref_time}</span></div>"#,
                    esc(&group.name), fmt_time_html(fad_row.median_ns),
                ).unwrap();
            }
        }

        h.push_str("</div>\n");
    }

    h.push_str("<footer>");
    write!(h, "{} · {} · {}", esc(&meta.datetime), esc(&meta.commit), esc(&meta.platform)).unwrap();
    h.push_str("</footer>\n");

    h.push_str(r#"<script>
function switchTab(id) {
  document.querySelectorAll('.tab').forEach(t => t.classList.toggle('active', t.getAttribute('onclick').includes("'" + id + "'")));
  document.querySelectorAll('.panel').forEach(p => p.classList.toggle('active', p.id === 'panel-' + id));
}
</script>
</body>
</html>
"#);

    h
}

/// Produce a CSS-safe ID from a section label (e.g., "json deser" → "json-deser").
fn tab_id(label: &str) -> String {
    label.replace(' ', "-")
}
