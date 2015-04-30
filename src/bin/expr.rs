use std::env;
use std::fmt;
use std::error;

use std::iter::Peekable;

#[derive(Debug)]
enum Error { ParseError, EvalError }

impl error::Error for Error {
    fn description(&self) -> &str {
        match self {
            &Error::ParseError => "invalid expression",
            &Error::EvalError => "invalid value"
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        (self as &error::Error).description().fmt(f)
    }
}

#[derive(Debug,Copy,Clone)]
enum BinOp {
    Mul, Mod, Div,
    Add, Sub,
    Lower, LowerEq, Equal, NEqual, GreaterEq, Greater,
}

#[derive(Debug)]
enum Expr {
    Literal(String),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
}

impl Expr {
    fn eval_binop(op: BinOp, a: i64, b: i64) -> Result<i64, Error> {
        match op {
            BinOp::Mul => Ok(a * b),
            BinOp::Mod => Ok(a % b),
            BinOp::Div => if b == 0 { Err(Error::EvalError) }
                               else { Ok(a / b) },
            BinOp::Add => Ok(a + b),
            BinOp::Sub => Ok(a - b),
            BinOp::Lower => Ok((a < b) as i64),
            BinOp::LowerEq => Ok((a <= b) as i64),
            BinOp::Equal => Ok((a == b) as i64),
            BinOp::NEqual => Ok((a != b) as i64),
            BinOp::Greater => Ok((a > b) as i64),
            BinOp::GreaterEq => Ok((a >= b) as i64),
        }
    }

    fn eval(&self) -> Result<String, Error> {
        match self {
            &Expr::Literal(ref x) => Ok(x.to_string()),
            &Expr::BinOp(op, ref ba, ref bb) => {
                let str_a = try!(ba.eval());
                let val_a = try!(str_a.parse().map_err(|_| Error::EvalError));
                let str_b = try!(bb.eval());
                let val_b = try!(str_b.parse().map_err(|_| Error::EvalError));
                Expr::eval_binop(op, val_a, val_b).map(|x| x.to_string())
            }
        }
    }
}

struct Parser<T> where T: Iterator<Item=String> {
    tokens: Peekable<T>,
    bin_ops: Vec<Vec<(&'static str, BinOp)>>,
}

impl <T> Parser<T> where T: Iterator<Item=String> {
    fn parse_atom(&mut self) -> Result<Expr, Error> {
        if self.tokens.peek() == Some(&"(".to_string()) {
            // try to parse a parenthesized subexpression
            self.tokens.next();
            let sub = try!(self.parse_expr());
            if self.tokens.next() != Some(")".to_string()) {
                return Err(Error::ParseError)
            }
            Ok(sub)
        } else {
            let tok = match self.tokens.next() {
                Some(x) => x,
                None    => return Err(Error::ParseError),
            };

            if tok.chars().all(|x| x.is_alphanumeric()) {
                Ok(Expr::Literal(tok))
            } else {
                Err(Error::ParseError)
            }
        }
    }

    fn parse_binop(&mut self, level: usize) -> Result<Expr, Error> {
        let sub_a: Expr =
            if level == 0 {
                try!(self.parse_atom())
            } else {
                try!(self.parse_binop(level-1))
            };

        let op_token = match self.tokens.peek() {
            Some(tok) => tok.to_string(),
            None => return Ok(sub_a),
        };

        // find appropriate BinOp in the operator table
        let op = match self.bin_ops[level].iter().find(|p| *p.0 == op_token) {
            Some(p) => p.1,
            None => return Ok(sub_a),
        };

        self.tokens.next();
        let sub_b = try!(self.parse_binop(level));
        Ok(Expr::BinOp(op, Box::new(sub_a), Box::new(sub_b)))
    }

    fn parse_expr(&mut self) -> Result<Expr, Error> {
        let idx = self.bin_ops.len() - 1;
        self.parse_binop(idx)
    }

    fn parse(&mut self) -> Result<Expr, Error> {
        let expr = try!(self.parse_expr());
        match self.tokens.peek() {
            Some(_) => Err(Error::ParseError), // unused input
            None    => Ok(expr)
        }
    }

    fn new(itr: T) -> Parser<T> {
        Parser {
            tokens: itr.peekable(),
            bin_ops: vec![
                vec![("*", BinOp::Mul), ("%", BinOp::Mod), ("/", BinOp::Div)],
                vec![("+", BinOp::Add), ("-", BinOp::Sub)],
                vec![("<", BinOp::Lower), ("<=", BinOp::LowerEq),
                     ("=", BinOp::Equal), ("!=", BinOp::NEqual),
                     (">", BinOp::Greater), (">=", BinOp::GreaterEq)],
            ],
        }
    }
}

fn main() {
    let expr = Parser::new(env::args().skip(1)).parse().unwrap();
    println!("{}", expr.eval().unwrap());
}
