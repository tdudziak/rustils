use std::env;
use std::fmt;
use std::error::Error;

#[derive(Debug)]
struct ParseError;

impl Error for ParseError {
    fn description(&self) -> &str {
        "invalid time interval"
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.description().fmt(f)
    }
}

fn parse_time(time: &str) -> Result<f32, ParseError> {
    let (str_amount, str_mult): (String, String) =
        time.chars().partition(|x| *x == '.' || x.is_numeric());

    let amount: f32 = match str::parse(&str_amount) {
        Ok(x) => x,
        Err(_) => return Err(ParseError),
    };

    let mult: f32 = if str_mult == "s" || str_mult == "" {
        1.0
    } else if str_mult == "m" {
        60.0
    } else if str_mult == "h" {
        60.0 * 60.0
    } else {
        return Err(ParseError)
    };

    Ok(amount * mult)
}

fn fail<T>(msg: &str) -> T {
    println!("sleep: {}", msg);
    println!("Usage: sleep NUMBER[s|m|h]");
    std::process::exit(1)
}

fn main() {
    let args: Vec<_> = env::args().skip(1).collect();

    if args.len() == 0 {
        fail("missing operand")
    } else if args.len() > 1 {
        fail("too many operands")
    }

    let seconds = parse_time(&args[0]).unwrap_or_else(|x| {
        fail(x.description())
    });

    std::thread::sleep_ms((1000.0 * seconds) as u32)
}
