use std::env;

fn main() {
    let mut separator = '\n';
    let mut names = std::vec::Vec::new();

    for arg in env::args().skip(1) {
        if arg.starts_with("-") {
            if arg == "-0"  || arg == "--null" {
                separator = '\0';
            } else {
                panic!("Invalid option");
            }
        } else {
            names.push(arg);
        }
    }

    if names.len() == 0 {
        for (key, value) in env::vars() {
            print!("{}={}{}", key, value, separator);
        }
    } else {
        for name in names {
            match env::var(&name) {
                Ok(value) => print!("{}{}", value, separator),
                Err(_) => panic!("Variable not in the environment"),
            }
        }
    }
}
