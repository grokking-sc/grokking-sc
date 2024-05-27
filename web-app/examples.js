
const ArithmeticExpressions = `
def fac(n)     := ifz(n,1,n*fac(n-1));
def prod(n,m)  := ifz(n,0,ifz(n-1,m,m+prod(n-1,m)));
def monus(n,m) := ifz(m,n,ifz(n,0,monus(n-1,m-1)));
def main       := monus(10,5);
`;

const FastMultiplication = `
def fmult(l) := label a {mult2(l;a)};
def mult2(l;a) := case l of { Nil => 1, Cons(x,xs) => ifz(x,jump(0,a),x*mult2(xs;a))};
def main := fmult(Cons(2,Cons(0,Cons(3,Cons(3,Nil)))));
`;

const LazyPair = `
def swapLazy(x) := cocase { fst=>x, snd=>x.fst};
def toTuple(x) := Tup(x.fst,x.snd);
def fromTuple(x) := case x of {Tup(a,b) => cocase { fst=>a, snd=>b }};

def main := toTuple(fromTuple(Tup(1,2)));
`;

const Lists = `
def map(f, l) := case l of {Nil=>Nil, Cons(x, xs) => Cons(f, map(f, xs))};
def map(f, l) := case l of {Nil=>Nil, Cons(x, xs) => Cons(f, map(f, xs))};
def multFast(x) := label a { case x of {Nil=>1, Cons(y, ys) => ifz(y,jump(0,a),y * multFast(ys))}};
def mult(x) := case x of {Nil=>1, Cons(y, ys) => y * mult(ys)};
def foldr(f,st,ls) := case ls of {Nil=>st,Cons(y,ys) => foldr(f,f y st,ys)};
def len(ls) := case ls of { Nil => 0, Cons(y,ys) => 1+len(ys)};

def sum := \\x => \\y => x+y; 
def main := len(Cons(1,Cons(2,Cons(3,Cons(4,Nil))))); 
`;

const Stream = `
def repeat(x) := cocase { hd=>x, tl=>repeat(x)};
def const1 := cocase { hd => 1, tl=>const1};

def main := const1.hd;
`;

const Tuples = `
def swap(x) := case x of {Tup(a, b) => Tup(b, a)};
def diag(x) := Tup(x,x);
def first(x) := case x of {Tup(a,b) => a};
def second(x) := case x of {Tup(a,b) => b};
def toList(x) := case x of {Tup(a,b) => Cons(a,Cons(b,Nil)) };

def main := toList(Tup(1,2));
`;


const all_examples={
'ArithmeticExpressions':ArithmeticExpressions,
'FastMultiplication':FastMultiplication,
'LazyPair':LazyPair,
'Lists':Lists,
'Stream':Stream,
'Tuples':Tuples,
};
