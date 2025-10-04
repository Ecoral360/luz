use clap::Parser;
use luz::luz::err::LuzError;
use luz::run_file;

#[derive(Parser)]
pub struct LuzcArgs {
    file: Option<String>,

    #[command(flatten)]
    verbosity: clap_verbosity_flag::Verbosity,
}

fn main() -> Result<(), LuzError> {
    let args = LuzcArgs::parse();

    env_logger::Builder::new()
        .filter_level(args.verbosity.into())
        .init();

    if let Some(file) = args.file {
        if let Err(err) = run_file(&file) {
            println!("{}", err);
        }
    } else {
        // run REPL
        luz::run_repl()?;
    }

    Ok(())
}
