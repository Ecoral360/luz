use luz::luz::err::LuzError;
use luz::run;

fn main() -> Result<(), LuzError> {
    run(r#"
if _VERSION ~= version then
  io.log.writer(1).stderr:write("This test suite is for ", version,
                  ", not for ", _VERSION, "\nExiting tests")
  return
end
"#)
    //     run(r#"
    //     y = (12 - 1) == z
    //     x = -12.12 * y
    // "#)
}
