module Core.Simplify where 

import Core.Syntax 
import Core.Substitution
import Data.List 
import Duality.Syntax (BinOp (..))


--performs eta reductions on a program
class Simplify a where 
  simplify :: a -> a 

instance Simplify (Prog a) where 
  simplify (MkProg dfs) = MkProg (simplify <$> dfs)

instance Simplify (Def a) where 
  simplify Def{name=nm, pargs=args, cargs=coargs, body=st} = Def {name=nm, pargs=args, cargs=coargs, body=simplify st}

instance Simplify Statement where 
  -- a mu abstraction in a cut can immediately be removed by substituting the consumer
  -- <mu cv1.st | c> -> st [c/cv1]
  simplify (Cut (Mu cv1 s) c) =  simplify (substCovar c cv1 s) 
  -- as with a mu abstration, a mu-tilde abstraction can be simplified by substituting the producer 
  -- <p | mu v.st> -> st[p/v]
  simplify (Cut p (MuTilde v2 s)) = simplify (substVar p v2 s) 
  -- a cut between constructor and case can be replaced by the corresponding right-hand side of the pattern
  -- <ctor(args) | case { ... ctor(vars) => st ...} -> st[args/vars]
  simplify (Cut (Constructor ct args coargs) (Case pts)) = case find (\pat -> xtor pat == ct) pts of 
    Nothing -> Cut (Constructor ct (simplify <$> args) (simplify <$> coargs)) (Case (simplify <$> pts)) 
    Just MkPattern{xtor=_,patv=vars,patcv=covars,patst=st} -> substSim (zip args vars) (zip coargs covars) st
  -- as with a constructor and case, a cocase and destructor can similarly be simplified
  simplify (Cut (Cocase pts) (Destructor dt args coargs)) =  case find (\pat -> xtor pat == dt) pts of
    Nothing -> Cut (Cocase (simplify <$> pts)) (Destructor dt (simplify <$> args) (simplify <$> coargs))
    Just MkPattern{xtor=_,patv=vars,patcv=covars,patst=st} -> substSim (zip args vars) (zip coargs covars) st
  -- otherwise simplifiy the producer and consumer in the cut 
  simplify (Cut p c) = Cut (simplify p) (simplify c)
  -- binary operations in literals can be immediately computed 
  simplify (Op (Lit n) Prod (Lit m) c) = Cut (Lit (n*m)) c
  simplify (Op (Lit n) Sum  (Lit m) c) = Cut (Lit (n+m)) c
  simplify (Op (Lit n) Sub  (Lit m) c) = Cut (Lit (n-m)) c
  -- otherwise simplify the arguments
  simplify (Op p1 op p2 c) = Op (simplify p1) op (simplify p2) (simplify c)
  -- ifzero 0 can be direclty shortcut to the first statement
  simplify (IfZ (Lit 0) s1 _) = simplify s1
  -- ifzero n /= 0 can be simplified to the second statemetn
  simplify (IfZ (Lit _) _ s2) = simplify s2
  -- otherwise simplify all arguments
  simplify (IfZ p s1 s2) = IfZ (simplify p) (simplify s1) (simplify s2)
  -- for a toplevel call, simplify all arguments
  -- this does not replace the call with it's body
  simplify (Fun nm args coargs) = Fun nm (simplify <$> args) (simplify <$> coargs)
  simplify Done = Done

instance Simplify (Pattern a) where 
  simplify MkPattern{xtor=xt,patv=vars,patcv=covars,patst=st} = MkPattern{xtor=xt,patv=vars,patcv=covars,patst=simplify st}


-- for producers and consumers no simlification is done, except recursively simplifying their arguments
instance Simplify Producer where 
  simplify (Var v) = Var v
  simplify (Lit n) = Lit n
  simplify (Mu cv st) = Mu cv (simplify st)
  simplify (Constructor ct args coargs) = Constructor ct (simplify <$> args) (simplify <$> coargs)
  simplify (Cocase pts) = Cocase (simplify <$> pts)

instance Simplify Consumer where 
  simplify (Covar cv) = Covar cv 
  simplify (MuTilde v st) = MuTilde v (simplify st)
  simplify (Case pts) = Case (simplify <$> pts)
  simplify (Destructor dt args coargs) = Destructor dt (simplify <$> args) (simplify <$> coargs)
