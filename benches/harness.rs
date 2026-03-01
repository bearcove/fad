//! Custom benchmark harness for fad.
//!
//! Emits NDJSON to stdout, human-readable progress to stderr.
//! Proper warmup, auto-batching, and percentile stats.
//!
//! Include from bench files: `#[path = "harness.rs"] mod harness;`

#![allow(dead_code)]

use std::cell::RefCell;
use std::hint::black_box;
use std::time::{Duration, Instant};

/// A registered benchmark.
pub struct Bench {
    pub name: String,
    pub func: Box<dyn Fn(&Runner)>,
}

/// Measurement controller. The benchmark closure calls `runner.run(|| { ... })`.
pub struct Runner {
    results: RefCell<Option<BenchResult>>,
}

/// Results for one benchmark.
struct BenchResult {
    median_ns: f64,
    p5_ns: f64,
    p95_ns: f64,
    min_ns: f64,
    max_ns: f64,
    samples: usize,
    iters_per_sample: usize,
}

impl Runner {
    fn new() -> Self {
        Self {
            results: RefCell::new(None),
        }
    }

    /// Run the measurement lifecycle: warmup → calibrate → measure → stats.
    pub fn run<F: Fn()>(&self, f: F) {
        // Phase 1: Warmup — 500ms or 1000 iters, whichever comes first.
        let warmup_deadline = Instant::now() + Duration::from_millis(500);
        let mut warmup_iters = 0u64;
        while Instant::now() < warmup_deadline && warmup_iters < 1000 {
            black_box(&f)();
            warmup_iters += 1;
        }

        // Phase 2: Calibrate — find iters_per_sample so each sample takes ≥1ms.
        let mut iters_per_sample = 1usize;
        loop {
            let start = Instant::now();
            for _ in 0..iters_per_sample {
                black_box(&f)();
            }
            let elapsed = start.elapsed();
            if elapsed >= Duration::from_millis(1) || iters_per_sample >= 1_000_000 {
                break;
            }
            iters_per_sample *= 2;
        }

        // Phase 3: Collect 100 samples.
        let num_samples = 100;
        let mut durations = Vec::with_capacity(num_samples);
        for _ in 0..num_samples {
            let start = Instant::now();
            for _ in 0..iters_per_sample {
                black_box(&f)();
            }
            durations.push(start.elapsed());
        }

        // Phase 4: Compute per-iteration nanoseconds.
        let mut per_iter_ns: Vec<f64> = durations
            .iter()
            .map(|d| d.as_nanos() as f64 / iters_per_sample as f64)
            .collect();
        per_iter_ns.sort_by(|a, b| a.partial_cmp(b).unwrap());

        *self.results.borrow_mut() = Some(BenchResult {
            median_ns: percentile(&per_iter_ns, 50.0),
            p5_ns: percentile(&per_iter_ns, 5.0),
            p95_ns: percentile(&per_iter_ns, 95.0),
            min_ns: per_iter_ns[0],
            max_ns: per_iter_ns[per_iter_ns.len() - 1],
            samples: num_samples,
            iters_per_sample,
        });
    }
}

fn percentile(sorted: &[f64], pct: f64) -> f64 {
    let idx = (pct / 100.0 * (sorted.len() - 1) as f64).round() as usize;
    sorted[idx.min(sorted.len() - 1)]
}

/// Run all benchmarks, applying CLI filter if present.
pub fn run_benchmarks(benchmarks: Vec<Bench>) {
    let filter = parse_filter();

    let filtered: Vec<&Bench> = benchmarks
        .iter()
        .filter(|b| match &filter {
            Some(f) => b.name.contains(f.as_str()),
            None => true,
        })
        .collect();

    eprintln!(
        "running {} benchmarks (of {} registered)",
        filtered.len(),
        benchmarks.len()
    );

    for bench in &filtered {
        eprint!("  {} ... ", bench.name);
        let runner = Runner::new();
        (bench.func)(&runner);

        let result = runner
            .results
            .borrow_mut()
            .take()
            .expect("bench did not call runner.run()");

        // NDJSON to stdout
        println!(
            r#"{{"name":"{}","median_ns":{:.1},"p5_ns":{:.1},"p95_ns":{:.1},"min_ns":{:.1},"max_ns":{:.1},"samples":{},"iters_per_sample":{}}}"#,
            bench.name,
            result.median_ns,
            result.p5_ns,
            result.p95_ns,
            result.min_ns,
            result.max_ns,
            result.samples,
            result.iters_per_sample,
        );

        // Human-readable to stderr
        eprintln!("{}", fmt_time(result.median_ns));
    }
}

fn parse_filter() -> Option<String> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    for i in 0..args.len() {
        if args[i] == "--filter" {
            return args.get(i + 1).cloned();
        }
        if let Some(val) = args[i].strip_prefix("--filter=") {
            return Some(val.to_string());
        }
    }
    // First non-flag arg is the filter (matches `cargo bench -- <filter>`)
    for arg in &args {
        if !arg.starts_with('-') {
            return Some(arg.clone());
        }
    }
    None
}

fn fmt_time(ns: f64) -> String {
    if ns >= 1_000_000.0 {
        format!("{:.2} ms", ns / 1_000_000.0)
    } else if ns >= 1_000.0 {
        format!("{:.2} µs", ns / 1_000.0)
    } else {
        format!("{:.1} ns", ns)
    }
}
