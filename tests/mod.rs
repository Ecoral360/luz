use luz::run_file;

#[test]
fn test_all() {
    let result = run_file("./tests/lua_tests/all.lua");

    result.expect("Error in test");
}
