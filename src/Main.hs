module Main where

  data Exp = Var String
             | Val Double
             | Add Exp Exp
             | Mul Exp Exp
  
  type Env = [(String, Double)] 
  
  eval :: Exp -> Env -> Double
  eval (Var x)   e = fetch x e
  eval (Val i)   e = i
  eval (Add p q) e = eval p e + eval q e  
  eval (Mul p q) e = eval p e * eval q e 
  
  k :: a -> env -> a
  k x e = x
  s :: (env -> a -> b) -> (env -> a) -> env -> b
  s ef es e = ef e (es e)
  
  eval1 :: Exp -> Env -> Double
  eval1 (Var x)   = fetch x
  eval1 (Val i)   = k i
  eval1 (Add p q) = k (+) `s` eval1 p `s` eval1 q  
  eval1 (Mul p q) = k (*) `s` eval1 p `s` eval1 q 
  
  -- instance Applicative ((->) r) where
  -- pure x _ = x
  -- f <*> g = \x -> f x (g x)
  
  eval2 :: Exp -> Env -> Double
  eval2 (Var x)   = fetch x
  eval2 (Val i)   = pure i
  eval2 (Add p q) = pure (+) <*> eval2 p  <*> eval2 q  
  eval2 (Mul p q) = pure (*) <*> eval2 p  <*> eval2 q 
  
  fetch :: String -> Env -> Double
  fetch x []        = error $ "variable " ++ x ++ " is not defined"
  fetch x ((y,v):ys)
    | x == y    = v
    | otherwise = fetch x ys
  
  main :: IO ()
  main = do
    let exp = Mul (Add (Val 3) (Val 4)) (Mul (Val 2) (Var "x"))
    let env = [("x", 2)]
    print $ eval2 exp env