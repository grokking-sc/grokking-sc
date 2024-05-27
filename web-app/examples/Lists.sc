def map(f, l) := case l of {Nil=>Nil, Cons(x, xs) => Cons(f, map(f, xs))};
def map(f, l) := case l of {Nil=>Nil, Cons(x, xs) => Cons(f, map(f, xs))};
def multFast(x) := label a { case x of {Nil=>1, Cons(y, ys) => ifz(y,jump(0,a),y * multFast(ys))}};
def mult(x) := case x of {Nil=>1, Cons(y, ys) => y * mult(ys)};
def foldr(f,st,ls) := case ls of {Nil=>st,Cons(y,ys) => foldr(f,f y st,ys)};
def len(ls) := case ls of { Nil => 0, Cons(y,ys) => 1+len(ys)};

def sum := \\x => \\y => x+y; 
def main := len(Cons(1,Cons(2,Cons(3,Cons(4,Nil))))); 
