module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Seccón 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

conversion :: LamTerm -> Term
conversion lt = case lt of 
                    LVar var -> Free (Global var)
                    App lt1 lt2 -> (conversion lt1) :@: (conversion lt2) 
                    Abs var lt1 -> Lam (replace var 0 (conversion lt1))
                    where replace v n a@(Bound m) = a
                          replace v n a@(Free (Global l)) | v == l = Bound n
                                                          | otherwise = a
                          replace v n (t1 :@: t2) = (replace v n t1) :@: (replace v n t2)
                          replace v n (Lam t) = Lam (replace v (n+1) t)
                 

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp (Vlam f) v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv)  = lEnv !! ii
eval' (Free v)   (gEnv, _)  = gEnv !! v
eval' (t1 :@: t2) e         = vapp (eval' t1 e) (eval' t2 e)
eval' (Lam t) (gEnv, lEnv)  = VLam (\val -> eval' t (gEnv, val:lEnv))

--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote = undefined






