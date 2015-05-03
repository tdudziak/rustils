use std::env;
use std::fmt;
use std::error;

use std::iter::Peekable;
use std::marker::PhantomData;

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

trait Parser {
    type Itr: Iterator;
    type Res;

    fn parse(&self, itr: &mut Peekable<Self::Itr>) -> Result<Self::Res, Error>;
}

impl<I: Iterator, O> Parser for Box<Parser<Itr=I,Res=O>> {
    type Itr = I;
    type Res = O;

    fn parse(&self, itr: &mut Peekable<Self::Itr>) -> Result<Self::Res, Error> {
        (**self).parse(itr)
    }
}

struct Token<T>(String, PhantomData<T>);

struct Literal<T>(PhantomData<T>);

struct PairParser<A: Parser, B: Parser>(A, B);

struct Operator<P: Parser, F>(String, F, P);

struct Map<P: Parser, Out, F: Fn(P::Res) -> Out>(P, F);

impl <P: Parser, Out, F: Fn(P::Res) -> Out> Parser for Map<P, Out, F> {
    type Itr = P::Itr;
    type Res = Out;

    fn parse(&self, itr: &mut Peekable<Self::Itr>) -> Result<Out, Error> {
        let &Map(ref b_parser, ref f) = self;
        b_parser.parse(itr).map(f)
    }
}

struct Alternative<A: Parser, B: Parser>(A, B);

impl <A, B> Parser for Alternative<A, B> where
    A: Parser,
    B: Parser<Itr=A::Itr, Res=A::Res>
{
    type Itr = A::Itr;
    type Res = A::Res;

    fn parse(&self, itr: &mut Peekable<Self::Itr>) -> Result<Self::Res, Error> {
        let &Alternative(ref p_a, ref p_b) = self;
        match p_a.parse(itr) {
            Ok(x) => return Ok(x),
            x @ Err(Error::ParseError) => x, // ignored
            Err(err) => return Err(err),
        };
        p_b.parse(itr).map_err(make_fatal)
    }
}

struct ParseAll<P: Parser>(P);

impl<P: Parser> Parser for ParseAll<P> {
    type Itr = P::Itr;
    type Res = P::Res;

    fn parse(&self, itr: &mut Peekable<Self::Itr>) -> Result<Self::Res, Error> {
        let &ParseAll(ref parser) = self;
        let res = try!(parser.parse(itr));
        match itr.peek() {
            Some(_) => Err(Error::FatalParseError),
            None => Ok(res)
        }
    }
}

struct OptionParser<P: Parser>(P);

impl<T: Iterator<Item=String>> Parser for Token<T> {
    type Itr = T;
    type Res = String;

    fn parse(&self, itr: &mut Peekable<Self::Itr>) -> Result<String, Error> {
        let &Token(ref self_tok, _) = self;
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

impl<T: Iterator<Item=String>> Parser for Literal<T> {
    type Itr = T;
    type Res = Expr;

    fn parse(&self, itr: &mut Peekable<Self::Itr>) -> Result<Expr, Error> {
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

impl <A, B> Parser for PairParser<A, B> where
    A: Parser,
    B: Parser<Itr=A::Itr>
{
    type Itr = A::Itr;
    type Res = (A::Res, B::Res);

    fn parse(&self, itr: &mut Peekable<Self::Itr>) -> Result<Self::Res, Error> {
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

impl <I, P, F> Parser for Operator<P, F> where
    I: Iterator<Item=String>,
    P: Parser<Itr=I, Res=Expr>,
    F: Fn(Box<Expr>, Box<Expr>) -> Expr
{
    type Itr = P::Itr;
    type Res = Expr;

    fn parse(&self, itr: &mut Peekable<Self::Itr>) -> Result<Expr, Error> {
        let &Operator(ref op_token, ref op_func, ref sub_parser) = self;
        let left = try!(sub_parser.parse(itr));
        let p_op = Token(op_token.clone(), PhantomData);
        match p_op.parse(itr) {
            Ok(x) => x,
            Err(Error::ParseError) => return Ok(left),
            Err(x) => return Err(x),
        };
        let right = try!(self.parse(itr).map_err(make_fatal));
        Ok(op_func(Box::new(left), Box::new(right)))
    }
}

impl <P: Parser> Parser for OptionParser<P> {
    type Itr = P::Itr;
    type Res = Option<P::Res>;

    fn parse(&self, itr: &mut Peekable<Self::Itr>) -> Result<Option<P::Res>, Error> {
        let &OptionParser(ref parser) = self;
        match parser.parse(itr) {
            Ok(x) => return Ok(Some(x)),
            Err(Error::ParseError) => return Ok(None),
            Err(e) => return Err(e)
        }
    }
}

struct ExprParser<T: Iterator<Item=String> + 'static>(PhantomData<T>);

impl <T: Iterator<Item=String> + 'static> Parser for ExprParser<T>
{
    type Itr = T;
    type Res = Expr;

    fn parse(&self, itr: &mut Peekable<T>) -> Result<Expr, Error> {
        // recursively parse '(' Expr ')'
        let recur = PairParser(
            Token("(".to_string(), PhantomData),
            PairParser(
                ExprParser(PhantomData),
                Token(")".to_string(), PhantomData)
            ));

        let atom = Alternative(Literal(PhantomData),
                               Map(recur, |(_,(x,_))| x));

        // TODO: boxing could be avoided using a macro
        let mut arith_expr: Box<Parser<Itr=T,Res=Expr>> = Box::new(atom);
        let arith_ops = vec![
            ("*", ArithOp::Mul), ("%", ArithOp::Mod), ("/", ArithOp::Div),
            ("+", ArithOp::Add), ("-", ArithOp::Sub),
            ("<", ArithOp::Lower), ("<=", ArithOp::LowerEq),
            ("=", ArithOp::Equal), ("!=", ArithOp::NEqual),
            (">", ArithOp::Greater), (">=", ArithOp::GreaterEq),
        ];
        for (op, ao) in arith_ops {
            let op_func = move |a,b| Expr::ArithOp(ao, a, b);
            arith_expr =
                Box::new(Operator(op.to_string(), op_func, arith_expr));
        }

        let and_expr =
            Operator("&".to_string(), |a,b| Expr::And(a,b), arith_expr);
        let or_expr =
            Operator("|".to_string(), |a,b| Expr::Or(a,b), and_expr);

        let parser = or_expr; // TODO: cache?
        parser.parse(itr)
    }
}

fn main() {
    let mut itr = env::args().skip(1).peekable();
    let parser = ParseAll(ExprParser(PhantomData));
    let expr = parser.parse(&mut itr).unwrap();
    println!("{}", expr.eval().unwrap());
}
