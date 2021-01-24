module Parser (
  parse
)
where
import Data.Char
import Control.Applicative
import LambdaTerm

newtype Parser a = P (String -> Maybe (a, String))

parse' :: Parser a -> String -> Maybe (a, String)
parse' (P p) = p

item :: Parser Char
item = P (\inp -> case inp of
             x:xs -> Just (x, xs)
             [] -> Nothing)
       
instance Functor Parser where
  fmap g p = P (\inp -> case parse' p inp of
                   Nothing -> Nothing
                   Just (v, out) -> Just (g v, out))
             
instance Applicative Parser where
  pure v = P (\inp -> Just (v, inp))
  pg <*> px = P (\inp -> case parse' pg inp of
                    Nothing -> Nothing
                    Just (g, out) -> parse' (fmap g px) out)

instance Monad Parser where       
  p >>= f = P (\inp -> case parse' p inp of
                  Nothing -> Nothing
                  Just (v, out) -> parse' (f v) out)
  return = pure
            
instance Alternative Parser where
  empty = P (const Nothing)
  p <|> q = P (\inp -> case parse' p inp of
                  Nothing -> parse' q inp
                  Just (v,out) -> Just (v,out))

--
sat :: (Char -> Bool) -> Parser Char  
sat p = do
  x <- item
  if p x then
    return x
    else
    empty
  
char :: Char -> Parser Char
char x = sat (== x)

digit :: Parser Char
digit = sat isDigit

varname :: Parser VarName -- ex. a,b, x1, y235 etc.
varname = do
  x <- item
  if isAlpha x then
    do
      xs <- many digit
      return $ x:xs
    else
    empty

var :: Parser Term -- variable
var = do
  x <- varname
  return $ Var x

app :: Parser Term -- application
app = do
  char '('
  m <- term
  n <- term
  char ')'
  return $ App m n

app' :: Parser Term -- abbrev. application ex. xy == (xy), xyz == ((xy)z)
app' = do
  t1 <- term
  t2 <- term
  ts <- many term
  return $ foldl App (App t1 t2) ts

term :: Parser Term
term = var <|> app <|> lambda

term' :: Parser Term
term' = app' <|> lambda' <|> term

lambda :: Parser Term -- lambda
lambda = do
  char '('
  t <- lambda'
  char ')'
  return t

lambda' :: Parser Term -- abbrev. lambda
lambda' = do
  char '\\'
  xs <- some varname
  char '.'
  m <- term'
  return $ lmd xs m
  where
    lmd [x0] m = Lmd x0 m
    lmd (x0:xs) m = Lmd x0 (lmd xs m)
    lmd [] _ = error "invalid lmd"

parse :: String -> Term
parse inp = case parse' term' inp of
  Just (t, []) -> t
  Just (t, out) -> error $ "parse " ++ show t ++ ", but out = " ++ show out
  Nothing -> error "fail to parse"

-- (a) xyz(yx) === (((xy)z)(yx))
-- (b) \x.uxy === (\x.((ux)y))
-- (c) \u.u(\x.y) === (\u.(u(\x.y)))
-- (d) (\u.vuu)zy === (((\u.((vu)u))z)y)
-- (e) ux(yz)(\v.vy) === (((ux)(yz))(\v.(vy)))
-- (f) (\xyz.xz(yz))uvw === ((((\x.(\y.(\z.((xz)(yz)))))u)v)w)
-- | parse test
-- >>> parse "x" == Var "x"
-- True
-- >>> parse "(xy)" == App (Var "x") (Var "y")
-- True
-- >>> parse "(\\x.y)" == Lmd "x" (Var "y")
-- True
-- >>> parse "(((xy)z)(yx))" == (App (App (App (Var "x") (Var "y")) (Var "z")) (App (Var "y") (Var "x")))
-- True
-- >>> parse "(\\x.((ux)y))" == Lmd "x" (App (App (Var "u") (Var "x")) (Var "y"))
-- True
-- >>> parse "(\\u.(u(\\x.y)))" == Lmd "u" (App (Var "u") (Lmd "x" (Var "y")))
-- True
-- >>> parse "(((\\u.((vu)u))z)y)" == (App (App (Lmd "u" (App (App (Var "v") (Var "u")) (Var "u"))) (Var "z")) (Var "y"))
-- True
-- >>> parse "(((ux)(yz))(\\v.(vy)))" == (App (App (App (Var "u") (Var "x")) ((App (Var "y") (Var "z")))) (Lmd "v" (App (Var "v") (Var "y"))))
-- True
-- >>> parse "((((\\x.(\\y.(\\z.((xz)(yz)))))u)v)w)" == (App (App (App (Lmd "x" (Lmd "y" (Lmd "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))) (Var "u")) (Var "v")) (Var "w"))
-- True

-- | parse abbreviation
-- >>> parse "xyz(yx)"
-- xyz(yx)
-- >>> parse "\\x.uxy"
-- \x.uxy
-- >>> parse "\\u.u(\\x.y)"
-- \u.u(\x.y)  
-- >>> parse "(\\u.vuu)zy"
-- (\u.vuu)zy
-- >>> parse "ux(yz)(\\v.vy)"
-- ux(yz)(\v.vy)
-- >>> parse "(\\xyz.xz(yz))uvw"
-- (\xyz.xz(yz))uvw
-- >>> parse "\\x.yz"
-- \x.yz
-- >>> parse "\\x.(yz)"
-- \x.yz
-- >>> parse "(\\x.(yz))"
-- \x.yz
