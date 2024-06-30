
const ArithmeticExpressions = `
// Factorial function
def fac(n) := ifz(n, 1, n * fac(n - 1));
def prod(n, m) := ifz(n, 0, ifz(n - 1, m, m + prod(n - 1, m)));

// Subtraction with cut-off, i.e. monus(4,2) = 2 and monus(2,4) = 0
def monus(n, m) := ifz(m, n, ifz(n, 0, monus(n - 1, m - 1)));

def main := monus(10, 5);
`;

const FastMultiplication = `
// Fast multiplication function from the introduction of the paper.
def fmult(l) := label a { mult2(l; a) };
def mult2(l; a) := case l of { Nil => 1,
                              Cons(x, xs) => ifz(x, goto(0; a), x * mult2(xs; a)) };
def main := fmult(Cons(2, Cons(0, Cons(3, Cons(3, Nil)))));
`;

const Lambdas = `
def nonValueArguments := (\\x => \\y => y) (1 + 2) (3 + 4);

def higherOrder := (\\x => \\y => x y) (\\z => 4 + z) (3 + 1);

def main := higherOrder();
`;

const LazyPair = `
// Swap the elements of a lazy pair.
def swapLazy(x) := cocase { fst => x.snd, snd => x.fst };

// Convert a lazy tuple to a strict tuple.
def toTuple(x) := Tup(x.fst, x.snd);

// Convert a strict tuple to a lazy tuple.
def fromTuple(x) := case x of { Tup(a, b) => cocase { fst => a, snd => b }};

def main := toTuple(fromTuple(Tup(1, 2)));
`;

const Lists = `
def map(f, l) := case l of { Nil => Nil,
                             Cons(x, xs) => Cons(f, map(f, xs)) };
def mult(x) := case x of { Nil => 1,
                           Cons(y, ys) => y * mult(ys) };
def foldr(f, st, ls) := case ls of { Nil => st,
                                     Cons(y, ys) => foldr(f, f y st, ys)};
def len(ls) := case ls of { Nil => 0,
                            Cons(y, ys) => 1 + len(ys)};

def main := len(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))));
`;

const Stream = `
def repeat(x) := cocase { hd => x, tl => repeat(x) };
def const1 := cocase { hd => 1, tl => const1() };

def main := repeat(1);
`;

const Tuples = `
def swap(x) := case x of { Tup(a, b) => Tup(b, a) };
def diag(x) := Tup(x, x);
def first(x) := case x of { Tup(a, b) => a };
def second(x) := case x of { Tup(a, b) => b };
def toList(x) := case x of { Tup(a, b) => Cons(a, Cons(b, Nil)) };

def main := toList(Tup(1, 2));
`;

const paper_examples = `
// example 2.1
def ex211 := 2 * 3;
def ex212 := ifz(2, 5, 10);

// example 2.2
def ex22 := let x = 2 * 2 in x * x;

// example 2.3
def fac(n) := ifz(n, 1, n * fac(n - 1));
def ex23 := fac(1);

// section 2.4
def sum(x) := case x of { Nil => 0,
                          Cons(y, ys) => y + sum(ys) };
def repeat(x) := cocase { hd => x, tl => repeat(x) };

// section 2.4.1, example 2.4
def swap(x) := case x of { Tup(y, z) => Tup(z, y) };

// section 2.4.2, example 2.5
def swaplazy(x) := cocase { fst => x.snd, snd => x.fst };

// example 2.6
def ex26 := (\\x => x * x) 2;

//example 2.7
def mult(l) := label a { mult2(l; a) };
def mult2(l; a) := case l of { Nil => 1,
                               Cons(x, xs) => ifz(x, goto(0; a), x * mult2(xs; a))};

// section 5.1
def sec51 := (2 * 3) * 4;

//section 5.3
def letex := let x = 2 in x * x;
def labelex := label a { goto(0; a) };

//section 5.4
def casecase := case (case Nil of { Nil => Nil, Cons(x, xs) => xs}) of {
                   Nil => Nil,
                   Cons(y, ys) => ys };

//section 5.5
def tltltl := repeat(1).tl.tl.tl;

//section 5.6
def criticalEta1(; b) := let x = \\y => goto(\\z => 1; b) y in \\z => 3;
def criticalEta2(; b) := let x = goto(\\z => 1; b) in \\z => 3;

//def main := ex211();
//def main := ex212();
//def main := ex22();
//def main := ex23();
//def main := sum(Cons(1, Cons(1, Cons(1, Nil))));
//def main := repeat(1);
//def main := swap(Tup(1, 2));
//def main := swaplazy(cocase { fst => 1, snd => 2 }).snd;
//def main := ex26();
//def main := mult(Cons(2, Cons(2, Cons(0, Cons(3, Nil)))));
//def main := sec51();
//def main := letex();
//def main := labelex();
//def main := casecase();
//def main := tltltl();
//def main := label b { criticalEta1(; b) };
def main := label b { criticalEta2(; b) };
`;


const all_examples={
'ArithmeticExpressions':ArithmeticExpressions,
'FastMultiplication':FastMultiplication,
'Lambdas':Lambdas,
'LazyPair':LazyPair,
'Lists':Lists,
'Stream':Stream,
'Tuples':Tuples,
'paper_examples':paper_examples,
};
