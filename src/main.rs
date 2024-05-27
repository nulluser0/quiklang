use std::{fs, io::Read, process};

use crate::frontend::lexer::tokenize;

mod frontend;

fn main() {
    println!("Hello, world!");

    let mut source = match fs::File::open("./code.quik") {
        Ok(result) => result,
        Err(err) => {
            println!("No ./code.quik file found. Err: {}. Exiting...", err);
            process::exit(2);
        }
    };
    let mut contents = String::new();
    match source.read_to_string(&mut contents) {
        Ok(result) => result,
        Err(err) => {
            println!("Cannot read ./code.quik as String. Err: {}. Exiting...", err);
            process::exit(3);
        }
    };

    let result = tokenize(contents);

    println!("{:#?}", result);


}
