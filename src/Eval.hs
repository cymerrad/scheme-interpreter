module Eval where

import           Parser
import           Data.Map                      as M

type Procedure = [LispExpr] -> LispExpr
newtype Error = E String
instance Show Error where
  show (E body) = show body

type Env = M.Map String Procedure

env :: Env
env = M.fromList []

eval :: LispExpr -> Either Error LispExpr
eval val@(Constant _       ) = Right val
eval val@(Symbol   _       ) = Right val
eval val@(Cons _ _         ) = Right val
eval val@(Vector     _     ) = Right val
eval val@(Quote      _     ) = Right val
eval (    Quasiquote symbol) = case evalQuasiquote symbol of
  Left  err    -> Left err
  Right quoted -> Right (Quote quoted)
eval (Unquote _       ) = Left (E "unquote not in a quasiquote")
eval (List    (x : xs)) = case eval x of
  Left  err    -> Left err
  Right eval'd -> case eval'd of
    Symbol name -> evalApplication name xs
    _           -> Left (E "expected a procedure")
eval val@(List []) = Right val

evalQuasiquote :: LispExpr -> Either Error LispExpr
evalQuasiquote val = case val of
  Unquote symbol  -> eval symbol
  List    symbols -> case mapM evalQuasiquote symbols of
    Left  err      -> Left err
    Right unquoted -> Right (List unquoted)
  _ -> Right val

evalApplication :: String -> [LispExpr] -> Either Error LispExpr
evalApplication name args = case M.lookup name env of
  Nothing        -> Left (E (name ++ " undefined"))
  Just procedure -> Right (procedure args)
