module Security where

import           Common
import           Data.List


------------------------ <Funciones de seguridad para AP> --------------------------------
{-Verifica que el AP este bien construido-}
apChecker :: Ap -> Estado
apChecker ap1 = checker [checkerInit, checkerAcept , checkerTrans] ap1 

{-Verifica que el estado inicial esta en los estados del AP-}                                 
checkerInit:: Ap -> Estado
checkerInit (Ap s _ _ _ i _) = checkerFun (elem i s) errores 0

{-Verifica que los estados de acpetación estan en los estados del AP-} 
checkerAcept:: Ap -> Estado
checkerAcept (Ap s _ _ _ _ ac) = checkerFun (and $ map (\x->elem x s) ac) errores 1

{-Verifica que las transiciones del AP tengan sentido-} 
checkerTrans:: Ap -> Estado
checkerTrans (Ap s si g t _ _) = checkerFun (and $ map (\(T a b c d e)-> and [elem a s,(elem b si) || b=='/',(elem c g )|| c=="/", (elem d g)|| d=="/",elem e s]) t) errores 2

{-Ajusta el AP eliminando duplicados-} 
apReconstructor:: Ap -> Ap
apReconstructor (Ap s si g t  i ac) = let
                                  newS  = nub s
                                  newSi = nub si
                                  newG  = nub $ filter (\x-> or (map (\(T a b c d e)-> x== c || x == d ) t )) g
                                  newT  = nub t  
                                  newAc = nub ac
                               in (Ap newS newSi newG newT  i newAc)

------------------------------------------------------------------------------------------


------------------------ <Funciones de seguridad para GI> --------------------------------
{-Verifica que la GI este bien construido-}
giChecker :: Gi -> Estado
giChecker gr1 = checker [checkerInitG,checkerRules] gr1 

{-Verifica que el no terminal inicial esta en los no terminales de la GI-}   
checkerInitG:: Gi -> Estado
checkerInitG (Gi n t p i)= checkerFun (elem i n) errores 3

{-Verifica que las reglas de predución de la GI tengan sentido-}   
checkerRules:: Gi -> Estado
checkerRules (Gi n t p i) = 
  checkerFun (and $ map (\(R non izq)-> elem non n 
  && (and $ map (\x-> elem x t || elem x n ) izq) ) p) errores 4


{-Ajusta la GI eliminando duplicados-} 
giReconstructor:: Gi -> Gi
giReconstructor (Gi n t p i) = let
                                  newN= nub $ filter (\x-> or (map (\(R a b)-> x== a || elem x b ) p )) n
                                  newT= nub t
                                  newP= nub p 
                               in Gi newN newT newP i
------------------------------------------------------------------------------------------


------------------------ <Funciones adicionales> --------------------------------

checker :: [(a -> Estado)]-> a -> Estado
checker [] _ =Okay
checker (f:fs) a = case f a of 
                     Error e -> Error e 
                     Okay ->checker fs a 

checkerFun:: Bool->[String]->Int-> Estado
checkerFun fun xs num=
    if fun
    then Okay
    else Error (xs !! num)                     

errores :: [String]
errores= ["Estado inicial no existente en los estados del AP\n",
           "Estados de aceptación no coincidentes con los estados del AP\n",
           "Transaciones no validas del AP\n",
           "No-terminal inicial existente en los no-terminales de la GI\n",
           "Reglas no validas de la GI\n"]   
------------------------------------------------------------------------------------------