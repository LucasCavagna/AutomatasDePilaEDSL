module Printer
  ( printAP )
where


import           Common
import           Text.PrettyPrint.HughesPJ
import           Prelude                 hiding ( (<>) )

cant::Int 
cant = 3

pp::Ap -> Doc
pp (Ap s si g t i ac) = 
    ppConj s "Estados = "
  <>ppConj (map (\x-> x :"") si) "Simbolos de Entrada = "
  <>ppConj  g  "Símbolos de Salida = "
  <>ppTrans (map transform t ) "Transiciones = {\n" 
  <>text "Estado inicial = "
  <>text i 
  <>text "\n"
  <>ppConj ac "Estados de aceptación = " 

transform :: Trans -> String
transform (T a b c d e) = concat ["(", a , "," , (b:""), "," , c ,"," ,d ,"," ,e, ")"]


ppConj::[String] -> String -> Doc
ppConj xs s = 
    text  s
  <>text "{"
  <>ppConj' xs
  <>text "}\n"

ppConj' :: [String] -> Doc
ppConj' [] = text ""
ppConj' [a]= text a
ppConj' (x:xs)   =    text x 
                   <> text "," 
                   <> ppConj' xs 

ppTrans :: [String] -> String -> Doc
ppTrans xs s = 
    text s
  <>ppTrans' xs cant
  <>text "}\n"

ppTrans' :: [String] -> Int -> Doc
ppTrans' [] _ = text ""
ppTrans' [a] _= text a
ppTrans' (x:xs) n | n==0 = text "\n" <> ppTrans' (x:xs) cant
                  | otherwise =    text x 
                                <> text "," 
                                <> ppTrans' xs (n-1)

printAP:: Ap->String->Doc
printAP  a name = 
    text ("<<<Automata " ++ name ++" >>>\n" )
  <>pp a