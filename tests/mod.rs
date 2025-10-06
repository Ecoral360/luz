use luz::parse_file;

#[test]
fn test_parse_all() {
    let result = parse_file("./tests/lua_tests/all.lua");

    result.expect("Error in test");
}

#[test]
fn test_good_parsing() {}
