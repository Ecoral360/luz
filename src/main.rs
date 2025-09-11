use luz::luz::err::LuzError;
use luz::run_file;

fn main() -> Result<(), LuzError> {
    run_file("./src/test-manual.lua")
}
