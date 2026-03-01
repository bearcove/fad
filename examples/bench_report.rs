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
    commit_short: String,
    commit_full: String,
    os_name: String,
    platform: String,
}

impl Meta {
    fn collect() -> Self {
        let datetime = sh("date", &["+%Y-%m-%d %H:%M %Z"]);
        let commit_short = sh("git", &["rev-parse", "--short", "HEAD"]);
        let commit_full = sh("git", &["rev-parse", "HEAD"]);
        let uname = sh("uname", &["-srm"]);
        let os_name = sh("uname", &["-s"]);
        let cpu = cpu_name();
        let platform = if cpu.is_empty() { uname } else { format!("{uname} · {cpu}") };
        Meta { datetime, commit_short, commit_full, os_name, platform }
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
    p5_ns: f64,
    p95_ns: f64,
}

// ── Parser (NDJSON) ───────────────────────────────────────────────────────

fn parse_ndjson(input: &str) -> Vec<Section> {
    let mut entries: Vec<(Vec<String>, f64, f64, f64)> = Vec::new();

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
        let p5_ns = v["p5_ns"].as_f64().unwrap_or(median_ns);
        let p95_ns = v["p95_ns"].as_f64().unwrap_or(median_ns);

        let parts: Vec<String> = name.split('/').map(String::from).collect();
        entries.push((parts, median_ns, p5_ns, p95_ns));
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
fn organize_entries(entries: &[(Vec<String>, f64, f64, f64)]) -> Vec<Section> {
    let mut sections: Vec<Section> = Vec::new();

    for (path, median_ns, p5_ns, p95_ns) in entries {
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

        group.rows.push(Row { name: row_name, median_ns: *median_ns, p5_ns: *p5_ns, p95_ns: *p95_ns });
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
    write!(j, r#"  "commit": "{}","#, meta.commit_short).unwrap();
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
                write!(j, r#"            {{ "name": "{}", "median_ns": {:.1}, "p5_ns": {:.1}, "p95_ns": {:.1} }}{comma}"#, row.name, row.median_ns, row.p5_ns, row.p95_ns).unwrap();
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
    writeln!(m, "> {} · {} · {}", meta.datetime, meta.commit_short, meta.platform).unwrap();
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

/// Map a ratio to a fill percentage of one half of the bar (0–50%).
fn ratio_to_fill(ratio: f64) -> f64 {
    let r = if ratio >= 1.0 { ratio } else { 1.0 / ratio };
    ((r - 1.0) / 3.0 * 50.0_f64).min(50.0)
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
  --fad:#34829c;--ref:#74d44a;
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
.ratio-col.win{color:var(--fad)}
.ratio-col.lose{color:var(--ref)}
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
  background:var(--track);
}
.delta-track::after{
  content:'';position:absolute;
  left:calc(50% - 0.5px);top:0;bottom:0;width:1px;
  background:rgba(255,255,255,0.2);z-index:1;pointer-events:none;
}
.delta-fill{position:absolute;top:0;bottom:0;opacity:0.55}
.delta-fill.fad-side{right:50%;background:var(--fad)}
.delta-fill.ref-side{left:50%;background:var(--ref)}
/* error bar whiskers */
.whisker{position:absolute;top:0;bottom:0;width:1px;z-index:2;background:rgba(255,255,255,0.8)}
.whisker-line{position:absolute;top:50%;height:1px;z-index:2;background:rgba(255,255,255,0.5)}
/* summary row — lives in the bar column */
.summary-cell{
  font-family:var(--sans);font-size:13px;font-weight:700;
  text-align:center;padding-bottom:6px;color:var(--text);
}
.summary-cell .fad-n{color:var(--fad)}
.summary-cell .ref-n{color:var(--ref)}
.summary-cell .sep{color:var(--dim);margin:0 8px}
/* footer */
footer{
  margin-top:24px;padding-top:12px;
  border-top:1px solid var(--border);
  font-size:12px;color:var(--dim);
}
footer a:hover{text-decoration:underline}
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
                r#"<div class="bench-row"><span></span><span></span><span></span><div class="summary-cell">fad wins <span class="fad-n">{fad_wins}</span><span class="sep">&middot;</span>{ref_name} wins <span class="ref-n">{serde_wins}</span></div><span></span></div>"#,
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
                let (bar_inner, ratio_html) = match ref_row {
                    Some(rr) => {
                        let ratio = rr.median_ns / fad_row.median_ns;
                        let fad_wins = ratio >= 1.0;
                        let fill = ratio_to_fill(ratio);
                        let fill_class = if fad_wins { "delta-fill fad-side" } else { "delta-fill ref-side" };
                        let ratio_cls = if fad_wins { "win" } else { "lose" };

                        // Whiskers: best/worst case ratios from p5/p95
                        let ratio_best = rr.p95_ns / fad_row.p5_ns; // best case for fad
                        let ratio_worst = rr.p5_ns / fad_row.p95_ns; // worst case for fad
                        let best_wins = ratio_best >= 1.0;
                        let worst_wins = ratio_worst >= 1.0;
                        let best_fill = ratio_to_fill(ratio_best);
                        let worst_fill = ratio_to_fill(ratio_worst);

                        let mut bar = format!(r#"<div class="{fill_class}" style="width:{fill:.1}%"></div>"#);

                        // Whisker line connecting worst to best case
                        if best_wins == worst_wins {
                            // Both on same side
                            let side = if best_wins { "fad-side" } else { "ref-side" };
                            let (lo, hi) = if worst_fill < best_fill { (worst_fill, best_fill) } else { (best_fill, worst_fill) };
                            if best_wins {
                                // fad side: right-anchored from 50%
                                write!(bar, r#"<div class="whisker-line {side}" style="right:calc(50% + {lo:.1}%);width:{:.1}%"></div>"#, hi - lo).unwrap();
                                write!(bar, r#"<div class="whisker {side}" style="right:calc(50% + {hi:.1}%)"></div>"#).unwrap();
                                write!(bar, r#"<div class="whisker {side}" style="right:calc(50% + {lo:.1}%)"></div>"#).unwrap();
                            } else {
                                // ref side: left-anchored from 50%
                                write!(bar, r#"<div class="whisker-line {side}" style="left:calc(50% + {lo:.1}%);width:{:.1}%"></div>"#, hi - lo).unwrap();
                                write!(bar, r#"<div class="whisker {side}" style="left:calc(50% + {hi:.1}%)"></div>"#).unwrap();
                                write!(bar, r#"<div class="whisker {side}" style="left:calc(50% + {lo:.1}%)"></div>"#).unwrap();
                            }
                        } else {
                            // Straddles the center line — draw each side separately
                            if best_wins {
                                write!(bar, r#"<div class="whisker-line fad-side" style="right:50%;width:{best_fill:.1}%"></div>"#).unwrap();
                                write!(bar, r#"<div class="whisker fad-side" style="right:calc(50% + {best_fill:.1}%)"></div>"#).unwrap();
                                write!(bar, r#"<div class="whisker-line ref-side" style="left:50%;width:{worst_fill:.1}%"></div>"#).unwrap();
                                write!(bar, r#"<div class="whisker ref-side" style="left:calc(50% + {worst_fill:.1}%)"></div>"#).unwrap();
                            } else {
                                write!(bar, r#"<div class="whisker-line ref-side" style="left:50%;width:{best_fill:.1}%"></div>"#).unwrap();
                                write!(bar, r#"<div class="whisker ref-side" style="left:calc(50% + {best_fill:.1}%)"></div>"#).unwrap();
                                write!(bar, r#"<div class="whisker-line fad-side" style="right:50%;width:{worst_fill:.1}%"></div>"#).unwrap();
                                write!(bar, r#"<div class="whisker fad-side" style="right:calc(50% + {worst_fill:.1}%)"></div>"#).unwrap();
                            }
                        }

                        (
                            bar,
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
                    r#"<div class="bench-row"><span class="bname">{}</span>{ratio_html}<span class="t-fad">{}</span><div class="delta-track">{bar_inner}</div><span class="t-ref">{ref_time}</span></div>"#,
                    esc(&group.name), fmt_time_html(fad_row.median_ns),
                ).unwrap();
            }
        }

        h.push_str("</div>\n");
    }

    h.push_str("<footer>");
    write!(h, "{} · ", esc(&meta.datetime)).unwrap();
    // Commit link to GitHub
    write!(h, r#"<a href="https://github.com/bearcove/fad/commit/{}" style="color:var(--dim);text-decoration:none">{}</a>"#,
        esc(&meta.commit_full), esc(&meta.commit_short)).unwrap();
    write!(h, " · ").unwrap();
    // OS icon
    h.push_str(os_icon(&meta.os_name));
    write!(h, " {}", esc(&meta.platform)).unwrap();
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

/// Inline SVG icon for the OS.
fn os_icon(os_name: &str) -> &'static str {
    match os_name {
        "Darwin" => r#"<svg style="vertical-align:-2px" width="14" height="14" viewBox="0 0 24 24" fill="currentColor"><path d="M18.71 19.5c-.83 1.24-1.71 2.45-3.05 2.47-1.34.03-1.77-.79-3.29-.79-1.53 0-2 .77-3.27.82-1.31.05-2.3-1.32-3.14-2.53C4.25 17 2.94 12.45 4.7 9.39c.87-1.52 2.43-2.48 4.12-2.51 1.28-.02 2.5.87 3.29.87.78 0 2.26-1.07 3.8-.91.65.03 2.47.26 3.64 1.98-.09.06-2.17 1.28-2.15 3.81.03 3.02 2.65 4.03 2.68 4.04-.03.07-.42 1.44-1.38 2.83M13 3.5c.73-.83 1.94-1.46 2.94-1.5.13 1.17-.34 2.35-1.04 3.19-.69.85-1.83 1.51-2.95 1.42-.15-1.15.41-2.35 1.05-3.11z"/></svg>"#,
        "Linux" => r#"<svg style="vertical-align:-2px" width="14" height="14" viewBox="0 0 24 24" fill="currentColor"><path d="M12.504 0c-.155 0-.315.008-.48.021-4.226.333-3.105 4.807-3.17 6.298-.076 1.092-.3 1.953-1.05 3.02-.885 1.051-2.127 2.75-2.716 4.521-.278.832-.41 1.684-.287 2.489a.424.424 0 00-.11.135c-.26.268-.45.6-.663.839-.199.199-.485.267-.797.4-.313.136-.658.269-.864.68-.09.189-.136.394-.132.602 0 .199.027.4.055.536.058.399.116.728.04.97-.249.68-.28 1.145-.106 1.484.174.334.535.47.94.601.81.2 1.91.135 2.774.6.926.466 1.866.67 2.616.47.526-.116.97-.464 1.208-.946.587-.003 1.23-.269 2.26-.334.699-.058 1.574.267 2.577.2.025.134.063.198.114.333l.003.003c.391.778 1.113 1.345 1.884 1.345.358 0 .705-.094 1.053-.283.591-.32.974-.77 1.143-1.272.17-.505.138-1.12-.164-1.768-.096-.2-.238-.381-.394-.556-.104-.131-.196-.237-.238-.356a.723.723 0 01-.024-.344c.018-.174.107-.377.224-.578.202-.362.483-.775.67-1.264l.003-.007c.183-.471.298-1.03.248-1.694-.045-.6-.228-1.278-.583-2.029-.177-.375-.385-.739-.63-1.1-.24-.35-.512-.669-.792-.982-.095-.104-.185-.21-.262-.337-.087-.145-.16-.293-.21-.439.005-.009.009-.019.009-.03.24-.682.359-1.515.359-2.485 0-1.252-.37-2.293-.978-3.036-.598-.733-1.378-1.096-2.078-1.096-.4 0-.766.108-1.055.287-.29.178-.556.45-.768.816a12.262 12.262 0 00-.48.891 10.078 10.078 0 00-.162.381.25.25 0 00-.026.07c-.15.3-.315.505-.464.633-.165.138-.31.192-.449.208-.096.01-.248-.04-.425-.192a2.474 2.474 0 01-.398-.441c-.3-.398-.657-.963-1.047-1.467-.39-.502-.844-.973-1.413-1.28-.57-.307-1.165-.438-1.843-.434z"/></svg>"#,
        _ if os_name.contains("Windows") || os_name.contains("MINGW") || os_name.contains("MSYS") => r#"<svg style="vertical-align:-2px" width="14" height="14" viewBox="0 0 24 24" fill="currentColor"><path d="M0 3.449L9.75 2.1v9.451H0m10.949-9.602L24 0v11.4H10.949M0 12.6h9.75v9.451L0 20.699M10.949 12.6H24V24l-12.9-1.801"/></svg>"#,
        _ => "",
    }
}

/// Produce a CSS-safe ID from a section label (e.g., "json deser" → "json-deser").
fn tab_id(label: &str) -> String {
    label.replace(' ', "-")
}
