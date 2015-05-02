use std::env;
use std::fmt;
use std::error;

use std::iter::Peekable;

#[derive(Debug)]
enum Error { ParseError, FatalParseError, EvalError }

fn make_fatal(err: Error) -> Error {
    match err {
        Error::ParseError => Error::FatalParseError,
        x => x,
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match self {
            &Error::ParseError => "invalid expression",
            &Error::FatalParseError => "invalid expression",
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

trait Parser<T, TRes> where T: Iterator {
    fn parse(&self, itr: &mut Peekable<T>) -> Result<TRes, Error>;
}

struct Token(String);

struct Literal;

struct PairParser<T, A, B>(Box<Parser<T, A>>, Box<Parser<T, B>>);

struct Operator<T> {
    op_token: String,
    op_func: Box<Fn(Box<Expr>, Box<Expr>) -> Expr>,
    sub_parser: Box<Parser<T, Expr>>
}

struct Map<T, F, A, B>(Box<Parser<T, A>>, F)
    where T: Iterator, F: Fn(A) -> B;

impl <T, F, A, B> Parser<T, B> for Map<T, F, A, B>
    where T: Iterator, F: Fn(A) -> B
{
    fn parse(&self, itr: &mut Peekable<T>) -> Result<B, Error> {
        let &Map(ref b_parser, ref f) = self;
        b_parser.parse(itr).map(f)
    }
}

fn operator<TFunc, T>(
        op_token: &str, op_func: TFunc, sub_parser: Box<Parser<T, Expr>>
    ) -> Operator<T> where TFunc: Fn(Box<Expr>, Box<Expr>) -> Expr + 'static {

    Operator {
        op_token: op_token.to_string(),
        op_func: Box::new(op_func),
        sub_parser: sub_parser,
    }
}

struct Alternative<T, TRes>(Box<Parser<T,TRes>>, Box<Parser<T,TRes>>)
    where T: Iterator;

impl <T, TRes> Parser<T, TRes> for Alternative<T, TRes> where T: Iterator
{
    fn parse(&self, itr: &mut Peekable<T>) -> Result<TRes, Error> {
        let &Alternative(ref p_a, ref p_b) = self;
        match p_a.parse(itr) {
            Ok(x) => return Ok(x),
            x @ Err(Error::ParseError) => x, // ignored
            Err(err) => return Err(err),
        };
        p_b.parse(itr).map_err(make_fatal)
    }
}

struct OptionParser<T, J> {
    sub_parser: Box<Parser<T, J>>
}

impl <T> Parser<T, String> for Token where T: Iterator<Item=String> {
    fn parse(&self, itr: &mut Peekable<T>) -> Result<String, Error> {
        let &Token(ref self_tok) = self;
        let tok: String = match itr.peek() {
            Some(x) => x.clone(),
            None => return Err(Error::ParseError),
        };

        if tok == self_tok.to_string() {
            itr.next();
            Ok(tok)
        } else {
            Err(Error::ParseError)
        }
    }
}

impl <T> Parser<T, Expr> for Literal where T: Iterator<Item=String> {
    fn parse(&self, itr: &mut Peekable<T>) -> Result<Expr, Error> {
        let tok = match itr.peek() {
            Some(x) => x.clone(),
            None    => return Err(Error::ParseError),
        };

        if tok.chars().all(|x| x.is_alphanumeric()) {
            itr.next();
            Ok(Expr::Literal(tok))
        } else {
            Err(Error::ParseError)
        }
    }
}

impl <T, A, B> Parser<T, (A, B)> for PairParser<T, A, B> where T: Iterator {
    fn parse(&self, itr: &mut Peekable<T>) -> Result<(A, B), Error> {
        let &PairParser(ref left, ref right) = self;
        let res_left = try!(left.parse(itr));

        // res_left already advanced the iterator so errors in res_right are
        // promoted to fatal
        let res_right = match right.parse(itr) {
            Err(Error::ParseError) => return Err(Error::FatalParseError),
            Err(x) => return Err(x),
            Ok(x) => x,
        };

        Ok((res_left, res_right))
    }
}

impl <T> Parser<T, Expr> for Operator<T> where T: Iterator<Item=String> {
    fn parse(&self, itr: &mut Peekable<T>) -> Result<Expr, Error> {
        let left = try!(self.sub_parser.parse(itr));
        let p_op = Token(self.op_token.clone());
        match p_op.parse(itr) {
            Ok(x) => x,
            Err(Error::ParseError) => return Ok(left),
            Err(x) => return Err(x),
        };
        let right = try!(self.parse(itr).map_err(make_fatal));
        Ok((self.op_func)(Box::new(left), Box::new(right)))
    }
}

impl <T, J> Parser<T, Option<J>> for OptionParser<T, J> where T: Iterator {
    fn parse(&self, itr: &mut Peekable<T>) -> Result<Option<J>, Error> {
        match self.sub_parser.parse(itr) {
            Ok(x) => return Ok(Some(x)),
            Err(Error::ParseError) => return Ok(None),
            Err(e) => return Err(e)
        }
    }
}

struct ExprParser<T>(std::marker::PhantomData<T>)
    where T: Iterator<Item=String> + 'static;

impl <T> Parser<T, Expr> for ExprParser<T>
    where T: Iterator<Item=String> + 'static
{
    fn parse(&self, itr: &mut Peekable<T>) -> Result<Expr, Error> {
        // recursively parse '(' Expr ')'
        let recur = PairParser(
            Box::new(Token("(".to_string())),
            Box::new(PairParser(
                Box::new(ExprParser(std::marker::PhantomData)),
                Box::new(Token(")".to_string()))
            )));

        let mut parser: Box<Parser<_, Expr>> =
            Box::new(Alternative(
                    Box::new(Literal),
                    Box::new(Map(Box::new(recur), |(_,(x,_))| x))));

        let arith_ops = vec![
            ("*", ArithOp::Mul), ("%", ArithOp::Mod), ("/", ArithOp::Div),
            ("+", ArithOp::Add), ("-", ArithOp::Sub),
            ("<", ArithOp::Lower), ("<=", ArithOp::LowerEq),
            ("=", ArithOp::Equal), ("!=", ArithOp::NEqual),
            (">", ArithOp::Greater), (">=", ArithOp::GreaterEq),
        ];
        for (op, ao) in arith_ops {
        let op_func = move |a,b| Expr::ArithOp(ao, a, b);
        parser = Box::new(operator(op, op_func, parser));
        }

        parser = Box::new(operator("&", |a,b| Expr::And(a,b), parser));
        parser = Box::new(operator("|", |a,b| Expr::Or(a,b), parser));

        parser.parse(itr)
    }
}

fn main() {
    let mut itr = env::args().skip(1).peekable();
    let parser = ExprParser(std::marker::PhantomData);
    let expr = parser.parse(&mut itr).unwrap();
    println!("{}", expr.eval().unwrap());
}
