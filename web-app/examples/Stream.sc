def repeat(x) := cocase { hd=>x, tl=>repeat(x)};
def const1 := cocase { hd => 1, tl=>const1};

def main := const1.hd;
