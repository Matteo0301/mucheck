{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}

module MuCheck.Utils ( selectMany
                     , selectOne
                     , relevantOps
                     , once
                     , once'
                     , selectIntOps
                     , selectIfElseBoolNegOps
                     , selectGuardedBoolNegOps) where

import Data.List(nub)
import Data.Generics (Data, Typeable, GenericM, gmapMo, everything, mkQ, mkMp)
import Language.Haskell.Exts (Literal(Int), Exp(App, Var, If), QName(UnQual), Name(Ident) , Stmt(Qualifier))
import qualified Language.Haskell.Exts.Syntax as Syntax
import MuCheck.MuOp
import Control.Monad (MonadPlus, mplus)

-- SYB functions
-- apply a mutating function on a piece of code once at a time
once :: MonadPlus m => GenericM m -> GenericM m
once f x = f x `mplus` gmapMo (once f) x

once' :: (forall a. Data a => a -> Maybe a) -> (forall a. Data a => a -> a)
once' f x = maybe x id $ once f x

-- select all code components satisfying a certain predicate
selectMany :: (Data a, Typeable b) => (b -> Bool) -> a -> [b]
selectMany f = everything (++) ([] `mkQ` keep f)
   where keep fn x = case (fn x) of
          True  -> [x]
          False -> []

-- special case of selectMany, which selects the first
-- components satisfying a predicate
selectOne f p = case (selectMany f p) of
                    [] -> Nothing
                    xs -> Just $ head xs

-- checking if a piece of code c is in the program p
p `contains` c = not . null $ selectMany (\c' -> c == c') p

-- selecting all relevant ops
relevantOps :: (Data a, Eq a) => a -> [MuOp] -> [MuOp]
relevantOps m = filter (relevantOp m)
  -- check if an operator can be applied to a program
  where relevantOp :: (Data a, Eq a) => a -> MuOp -> Bool
        relevantOp m op = once (mkMp' op) m /= Nothing

-- Define all operations on a value
selectValOps :: (Data a, Eq a, Typeable b, Mutable b, Eq b) => (b -> Bool) -> [b -> b] -> a -> [MuOp]
selectValOps p fs m = let vals = nub $ selectMany p m
                      in concatMap (\x -> x ==>* map (\f -> f x) fs) vals

selectValOps' :: (Data a, Eq a, Typeable b, Mutable b) => (b -> Bool) -> (b -> [b]) -> a -> [MuOp]
selectValOps' p f m = let vals = selectMany p m
                      in concatMap (\x -> x ==>* f x) vals
                      
selectIntOps :: (Data a, Eq a) => a -> [MuOp]
selectIntOps = selectValOps isInt [\(Int i) -> Int (i + 1)
                                 , \(Int i) -> Int (i - 1)
                                 , \(Int i) -> if (abs i /= 1) then Int 0 else Int i
                                 , \(Int i) -> if (abs (i-1) /= 1) then Int 1 else Int i]
                    where isInt (Int _) = True
                          isInt _       = False

-- negating boolean in if/else statements
selectIfElseBoolNegOps :: (Data a, Eq a) => a -> [MuOp]
selectIfElseBoolNegOps = selectValOps isIf [\(If e1 e2 e3) -> If (App (Var (UnQual (Ident "not"))) e1) e2 e3]
                            where isIf (If _ _ _) = True
                                  isIf _          = False

-- negating boolean in Guards
selectGuardedBoolNegOps :: (Data a, Eq a) => a -> [MuOp]
selectGuardedBoolNegOps = selectValOps' isGuardedRhs negateGuardedRhs
                              where isGuardedRhs (Syntax.GuardedRhs _ _ _) = True
                                    boolNegate e@(Qualifier (Var (UnQual (Ident "otherwise")))) = [e]
                                    boolNegate (Qualifier exp) = [Qualifier (App (Var (UnQual (Ident "not"))) exp)]
                                    boolNegate x = [x]
                                    negateGuardedRhs (Syntax.GuardedRhs srcLoc stmts exp)
                                        = let stmtss = once (mkMp boolNegate) stmts
                                          in [Syntax.GuardedRhs srcLoc s exp | s <- stmtss]

