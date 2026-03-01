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
    let decoder = kajit::compile_decoder(StringBag::SHAPE, &kajit::json::KajitJson);

    for _ in 0..100_000 {
        let result: StringBag = kajit::from_str(&decoder, std::hint::black_box(&data)).unwrap();
        std::hint::black_box(&result);
    }
}
