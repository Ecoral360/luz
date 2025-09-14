use clap::Parser;
use luz::luz::err::LuzError;
use luz::run_file;

#[derive(Parser)]
pub struct LuzcArgs {
    file: String,

    #[command(flatten)]
    verbosity: clap_verbosity_flag::Verbosity,
}

fn main() -> Result<(), LuzError> {
    let args = LuzcArgs::parse();

    env_logger::Builder::new()
        .filter_level(args.verbosity.into())
        .init();

    run_file(&args.file)
}
