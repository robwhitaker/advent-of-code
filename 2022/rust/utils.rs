pub fn throw<R>(err: &str) -> R {
    Err("").expect(err)
}
