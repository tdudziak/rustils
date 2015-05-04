use std::env;
use std::fmt;
use std::error;
use std::thread;

use std::iter::Peekable;

#[derive(Debug)]
struct Error(String);

impl Error {
    fn new<T: fmt::Debug>(expected: &'static str, found: T) -> Error {
        Error(format!("invalid expression: expected {} but found {:?}",
                      expected, found))
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        "invalid expression"
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let &Error (ref msg) = self;
        msg.fmt(f)
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
    fn eval_arith(op: ArithOp, a: i64, b: i64) -> i64 {
        match op {
            ArithOp::Mul => a * b,
            ArithOp::Mod => a % b,
            ArithOp::Div => if b == 0 { panic!("division by zero") }
                               else { a / b },
            ArithOp::Add => a + b,
            ArithOp::Sub => a - b,
            ArithOp::Lower => (a < b) as i64,
            ArithOp::LowerEq => (a <= b) as i64,
            ArithOp::Equal => (a == b) as i64,
            ArithOp::NEqual => (a != b) as i64,
            ArithOp::Greater => (a > b) as i64,
            ArithOp::GreaterEq => (a >= b) as i64,
        }
    }

    fn bool_val(x: &str) -> bool {
        x.parse().map(|x: i64| x != 0).unwrap_or(x != "")
    }

    fn eval(&self) -> String {
        match self {
            &Expr::Literal(ref x) => x.to_string(),
            &Expr::ArithOp(op, ref ba, ref bb) => {
                let pnum = |x: String| {
                    x.parse()
                     .map_err(|err| panic!("`{}' is not a number: {}", x, err))
                     .unwrap()
                };
                Expr::eval_arith(op, pnum(ba.eval()), pnum(bb.eval())).to_string()
            },
            &Expr::And(ref ba, ref bb) => {
                let val_a = ba.eval();
                let val_b = bb.eval();
                if Expr::bool_val(&val_a) && Expr::bool_val(&val_b) {
                    val_a
                } else {
                    "0".to_string()
                }
            }
            &Expr::Or(ref ba, ref bb) => {
                let val_a = ba.eval();
                let val_b = bb.eval();
                if Expr::bool_val(&val_a) { val_a.to_string() } else { val_b.to_string() }
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
            let found = self.tokens.next()
                                   .unwrap_or("end of input".to_string());
            if found != ")".to_string() {
                // TODO: use a macro for this
                return Err(Error::new("a closing paren", found))
            }
            Ok(sub)
        } else {
            let tok = match self.tokens.next() {
                Some(x) => x,
                None    => return Err(Error::new("an expression",
                                                 "end of input")),
            };

            if tok.chars().all(|x| x.is_alphanumeric()) {
                Ok(Expr::Literal(tok))
            } else {
                Err(Error::new("an identifier or a number", tok))
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
            Some(x) => Err(Error::new("end of input", x)),
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
    let expr = Parser::new(env::args().skip(1)).parse();
    let eval_thread = thread::spawn(|| {
        let result = expr.unwrap_or_else(|x| panic!("{}", x)).eval();
        println!("{}", result)
    });
    match eval_thread.join() {
        Ok(_) => (),
        Err(err) => {
            let err_msg: Option<&String> = err.downcast_ref();
            println!("error: {}", err_msg.unwrap())
        }
    }
}
