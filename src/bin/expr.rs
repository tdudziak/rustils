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
enum ArithOp {
    Mul, Mod, Div,
    Add, Sub,
    Lower, LowerEq, Equal, NEqual, GreaterEq, Greater,
}

#[derive(Debug)]
enum Expr {
    Literal(String),
    ArithOp(ArithOp, Box<Expr>, Box<Expr>), // arithmetic binary operators
    And(Box<Expr>, Box<Expr>), // `&' operator (works both on strings & ints)
    Or(Box<Expr>, Box<Expr>),  // `|' operator (works both on strings & ints)
}

impl Expr {
    fn eval_arith(op: ArithOp, a: i64, b: i64) -> Result<i64, Error> {
        match op {
            ArithOp::Mul => Ok(a * b),
            ArithOp::Mod => Ok(a % b),
            ArithOp::Div => if b == 0 { Err(Error::EvalError) }
                               else { Ok(a / b) },
            ArithOp::Add => Ok(a + b),
            ArithOp::Sub => Ok(a - b),
            ArithOp::Lower => Ok((a < b) as i64),
            ArithOp::LowerEq => Ok((a <= b) as i64),
            ArithOp::Equal => Ok((a == b) as i64),
            ArithOp::NEqual => Ok((a != b) as i64),
            ArithOp::Greater => Ok((a > b) as i64),
            ArithOp::GreaterEq => Ok((a >= b) as i64),
        }
    }

    fn bool_val(x: &str) -> bool {
        x.parse().map(|x: i64| x != 0).unwrap_or(x != "")
    }

    fn eval(&self) -> Result<String, Error> {
        match self {
            &Expr::Literal(ref x) => Ok(x.to_string()),
            &Expr::ArithOp(op, ref ba, ref bb) => {
                let str_a = try!(ba.eval());
                let val_a = try!(str_a.parse().map_err(|_| Error::EvalError));
                let str_b = try!(bb.eval());
                let val_b = try!(str_b.parse().map_err(|_| Error::EvalError));
                Expr::eval_arith(op, val_a, val_b).map(|x| x.to_string())
            }
            &Expr::And(ref ba, ref bb) => {
                let val_a = try!(ba.eval());
                let val_b = try!(bb.eval());
                if Expr::bool_val(&val_a) && Expr::bool_val(&val_b) {
                    Ok(val_a)
                } else {
                    Ok("0".to_string())
                }
            }
            &Expr::Or(ref ba, ref bb) => {
                let val_a = try!(ba.eval());
                let val_b = try!(bb.eval());
                if Expr::bool_val(&val_a) { Ok(val_a) } else { Ok(val_b) }
            }
        }
    }
}

struct Parser<T> where T: Iterator<Item=String> {
    tokens: Peekable<T>,
    bin_ops: Vec<Vec<(&'static str, ArithOp)>>,
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

    fn parse_arith_op(&mut self, level: usize) -> Result<Expr, Error> {
        let sub_a: Expr =
            if level == 0 {
                try!(self.parse_atom())
            } else {
                try!(self.parse_arith_op(level-1))
            };

        let op_token = match self.tokens.peek() {
            Some(tok) => tok.to_string(),
            None => return Ok(sub_a),
        };

        // find appropriate ArithOp in the operator table
        let op = match self.bin_ops[level].iter().find(|p| *p.0 == op_token) {
            Some(p) => p.1,
            None => return Ok(sub_a),
        };

        self.tokens.next();
        let sub_b = try!(self.parse_arith_op(level));
        Ok(Expr::ArithOp(op, Box::new(sub_a), Box::new(sub_b)))
    }

    fn parse_arith(&mut self) -> Result<Expr, Error> {
        let idx = self.bin_ops.len() - 1;
        self.parse_arith_op(idx)
    }

    fn parse_and(&mut self) -> Result<Expr, Error> {
        let sub_a = try!(self.parse_arith());

        let op_token = match self.tokens.peek() {
            Some(tok) => tok.to_string(),
            None => return Ok(sub_a),
        };

        if op_token != "&" {
            return Ok(sub_a)
        }

        self.tokens.next();
        let sub_b = try!(self.parse_and());
        Ok(Expr::And(Box::new(sub_a), Box::new(sub_b)))
    }

    fn parse_expr(&mut self) -> Result<Expr, Error> {
        let sub_a = try!(self.parse_and());

        let op_token = match self.tokens.peek() {
            Some(tok) => tok.to_string(),
            None => return Ok(sub_a),
        };

        if op_token != "|" {
            return Ok(sub_a)
        }

        self.tokens.next();
        let sub_b = try!(self.parse_and());
        Ok(Expr::Or(Box::new(sub_a), Box::new(sub_b)))
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
                vec![("*", ArithOp::Mul), ("%", ArithOp::Mod), ("/", ArithOp::Div)],
                vec![("+", ArithOp::Add), ("-", ArithOp::Sub)],
                vec![("<", ArithOp::Lower), ("<=", ArithOp::LowerEq),
                     ("=", ArithOp::Equal), ("!=", ArithOp::NEqual),
                     (">", ArithOp::Greater), (">=", ArithOp::GreaterEq)],
            ],
        }
    }
}

fn main() {
    let expr = Parser::new(env::args().skip(1)).parse().unwrap();
    println!("{}", expr.eval().unwrap());
}
