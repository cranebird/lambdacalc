module LambdaTerm (
  Term(Var, Lmd, App),
  VarName,
  showTerm
)
where

type VarName = String

data Term = Var VarName -- Variable
          | Lmd VarName Term -- Lambda Abstraction
          | App Term Term -- Application
          deriving (Eq)

-- simple printer
showTerm :: Term -> String
showTerm (Var x) = x
showTerm (Lmd x body) = "(\\" ++ x ++ "." ++ showTerm body ++ ")"
showTerm (App x y) = "(" ++ showTerm x ++ showTerm y ++ ")"

-- pretty printer
pp :: Term -> String
pp = pp' False
  where
    lambdas :: Term -> ([VarName], Term)
    lambdas (Lmd x (Var y)) = ([x], Var y)
    lambdas (Lmd x y) = case lambdas y of 
      ([], t) -> ([x], t)
      (ys, t) -> (x:ys, t)
    lambdas x = ([], x)
    
    apps :: Term -> [Term]
    apps (App x y) = apps x ++ [y]
    apps x = [x]
    
    pp' :: Bool -> Term -> String
    pp' _ (Var x) = x
    pp' b  (Lmd x y) = paren b $ "\\" ++ concat xs ++ "." ++ pp' False z
      where (xs, z) = lambdas (Lmd x y)
    pp' b (App x y) = paren b $ concatMap (pp' True) (apps x) ++ pp' True y
    
    paren :: Bool -> String -> String
    paren True x = "(" ++ x ++ ")"
    paren False x = x

instance Show Term where
    show = pp

-- |
-- >>> App (Var "x") (Var "y")    
-- xy
-- >>> App (App (App (Var "M") (Var "N")) (Var "P")) (Var "Q")
-- MNPQ
-- >>> Lmd "x" (App (Var "P") (Var "Q"))
-- \x.PQ
-- >>> Lmd "x1" (Lmd "x2" (Lmd "xn" (Var "M")))
-- \x1x2xn.M
    
-- Exercise 1.4
-- (a) xyz(yx) === (((xy)z)(yx))
-- (b) \x.uxy === (\x.((ux)y))
-- (c) \u.u(\x.y) === (\u.(u(\x.y)))
-- (d) (\u.vuu)zy === (((\u.((vu)u))z)y)
-- (e) ux(yz)(\v.vy) === (((ux)(yz))(\v.(vy)))
-- (f) (\xyz.xz(yz))uvw === ((((\x.(\y.(\z.((xz)(yz)))))u)v)w)
-- |
-- >>> (App (App (App (Var "x") (Var "y")) (Var "z")) (App (Var "y") (Var "x")))
-- xyz(yx)
-- >>> Lmd "x" (App (App (Var "u") (Var "x")) (Var "y"))
-- \x.uxy
-- >>> Lmd "u" (App (Var "u") (Lmd "x" (Var "y")))
-- \u.u(\x.y)
-- >>> (App (App (Lmd "u" (App (App (Var "v") (Var "u")) (Var "u"))) (Var "z")) (Var "y"))
-- (\u.vuu)zy
-- >>> (App (App (App (Var "u") (Var "x")) ((App (Var "y") (Var "z")))) (Lmd "v" (App (Var "v") (Var "y"))))
-- ux(yz)(\v.vy)    
-- >>> (App (App (App (Lmd "x" (Lmd "y" (Lmd "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))) (Var "u")) (Var "v")) (Var "w"))
-- (\xyz.xz(yz))uvw    
-- >>> Var "abc"
-- abc
-- >>> Lmd "x" (Var "x")
-- \x.x
-- >>> App (Var "x") (Var "y")
-- xy

