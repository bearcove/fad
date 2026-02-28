use serde::Deserialize;

#[derive(Debug, Deserialize, serde::Serialize)]
struct StringBag {
    values: Vec<String>,
}

fn main() {
    let bag = StringBag {
        values: (0..1024)
            .map(|i| format!("user-{i:04}-alpha-beta-gamma-delta-epsilon-zeta-theta-lambda"))
            .collect(),
    };
    let data = serde_json::to_string(&bag).unwrap();

    for _ in 0..100_000 {
        let result: StringBag = serde_json::from_str(std::hint::black_box(&data)).unwrap();
        std::hint::black_box(&result);
    }
}
