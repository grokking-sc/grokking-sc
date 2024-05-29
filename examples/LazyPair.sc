def swapLazy(x) := cocase { fst=>x, snd=>x.fst};
def toTuple(x) := Tup(x.fst,x.snd);
def fromTuple(x) := case x of {Tup(a,b) => cocase { fst=>a, snd=>b }};

def main := toTuple(fromTuple(Tup(1,2)));
