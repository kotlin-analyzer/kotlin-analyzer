use std::env;
use std::error::Error;
use std::process::Command;

fn main() -> Result<(), Box<dyn Error>> {
    gen_grammar_for_file("KotlinParser.g4", &["-visitor"])?;

    println!("cargo:rerun-if-changed=build/main.rs");
    Ok(())
}

fn gen_grammar_for_file(file_name: &str, additional_args: &[&str]) -> Result<(), Box<dyn Error>> {
    let input = env::current_dir()?.join("kotlin_spec/grammar/src/main/antlr");
    let antlr_path = env::current_dir()?.join("build/generator/antlr4-4.8-2-SNAPSHOT-complete.jar");
    let out_dir = env::var_os("OUT_DIR").ok_or_else(|| "OUT_DIR does not exist as env variable")?;

    Command::new("java")
        .current_dir(input)
        .arg("-cp")
        .arg(antlr_path)
        .arg("org.antlr.v4.Tool")
        .arg("-Dlanguage=Rust")
        .arg("-o")
        .arg(out_dir)
        .arg(&file_name)
        .args(additional_args)
        .spawn()?
        .wait_with_output()?;

    Ok(())
}
