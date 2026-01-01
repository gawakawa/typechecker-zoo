use clap::Parser;

use algorithm_w::{
    infer::{infer_type_only, run_inference},
    parser,
};

#[derive(Parser)]
#[command(name = "algorithm-w")]
struct Cli {
    expression: String,
}

fn main() {
    let cli = Cli::parse();
    run_single_expression(&cli.expression);
}

fn run_single_expression(input: &str) {
    let expr = match parser::ExprParser::new().parse(input) {
        Ok(expr) => expr,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            std::process::exit(1);
        }
    };

    println!("Parsed expression: {}", expr);
    println!();

    match (infer_type_only(&expr), run_inference(&expr)) {
        (Ok(final_type), Ok(tree)) => {
            println!("Type inference successful!");
            println!("Final type: {}", final_type);
            println!();
            println!("Inference trace:");
            println!("{}", tree);
        }
        (Err(e), _) | (_, Err(e)) => {
            eprintln!("Type inference error: {}", e);
            std::process::exit(1);
        }
    }
}
