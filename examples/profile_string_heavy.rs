use facet::Facet;

#[derive(Debug, Facet, serde::Serialize)]
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
    let decoder = fad::compile_decoder(StringBag::SHAPE, &fad::json::FadJson);

    for _ in 0..100_000 {
        let result: StringBag = fad::from_str(&decoder, std::hint::black_box(&data)).unwrap();
        std::hint::black_box(&result);
    }
}
