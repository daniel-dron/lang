fn bar() -> f64{
  return 10;
}

fn init() -> str {
    let foo: str = "hello";
    foo = bar();
    return "Hello World";
}