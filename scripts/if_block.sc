fn init() {
    let a = 10;
    let b = 11;

    let result = false;

    if a == b {
        result = "Equal";
    } else if a > b {
        result = "Bigger";
    } else if a < b {
        result = "Smaller";
    } else {
        result = "Idk lol";
    }

    return result;
}
