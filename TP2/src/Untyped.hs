module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Sección 2  
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
vapp (VLam f) v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv)  = lEnv !! ii
eval' (Free var) (gEnv, _)  = search var gEnv 
                                    where search v [] = VNeutral (NFree v)
                                          search v ((name,val):xs) = if v == name then val else search v xs
eval' (t1 :@: t2) e         = vapp (eval' t1 e) (eval' t2 e)
eval' (Lam t) (gEnv, lEnv)  = VLam (\val -> eval' t (gEnv, val:lEnv))

--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote value = quote' value 0

quote' :: Value -> Int -> Term
quote' (VLam fun) n                       = let val = (fun (VNeutral (NFree (Quote n))))
                                                t = quote' val (n+1)
                                                in Lam t
quote' (VNeutral (NFree (Global var))) _  = Free (Global var)
quote' (VNeutral (NFree (Quote m))) n     = Bound (n - m - 1)
quote' (VNeutral (NApp neutral val)) n    = let t1 = (quote' (VNeutral neutral) n)
                                                t2 = (quote' val n)
                                            in t1 :@: t2







