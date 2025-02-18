module ApFunctions
  (execute,oper, grammarToAp,adjustStates
  )
where 

import           Common
import           Data.List       

------------------------ <Comninadores unarios> -----------------------------------------

{-Crea un autómata de pila que acepta el lenguaje L^* , 
  donde L es el lenguaje aceptado por el autómata argumento-}
kleeneStarOp:: AP -> AP 
kleeneStarOp a =
  let (AP n (Ap s si g t i ac)) = emptyStack a
      newAc = i:ac 
      newT = (T (head ac) '/' "/" "/" i ) : t
  in (AP ('*':n) (Ap s si g newT i newAc))

{-Crea un autómata de pila que acepta el lenguaje L^+ , 
  donde L es el lenguaje aceptado por el autómata argumento-}
kleenePlusOp:: AP -> AP 
kleenePlusOp a@(AP name ap1) =
  let (AP n (Ap s si g t i ac)) = emptyStack a
      newT = (T (head ac) '/' "/" "/" i ) : t
  in (AP ('+':n) (Ap s si g newT i ac))

{-Crea un autómata de pila que acepta el lenguaje L^R , 
  donde L es el lenguaje aceptado por el autómata argumento-}
reverseOpAp:: AP -> AP
reverseOpAp  (AP name a ) = 
  let ap = adjustGamma $ grammarToAp $ apToGrammar a 
  in (AP ('R':name) ap)     


potencyOp ::AP -> Int -> AP
potencyOp a 1 = a
potencyOp a n = concatOp a (potencyOp a (n-1)) 

---------------------------------------------------------------------------------


------------------------ <Comninadores binarios> -----------------------------------------
{-Crea un autómata de pila que acepta el lenguaje L1 Union L2  , 
  donde L1 y L2 son los lenhuajes aceptados por los autómatas argumento-}
unionOp:: AP->AP-> AP
unionOp a@(AP n1 p1) (AP n2 p2)| n1 == n2 = a
unionOp (AP n1 (Ap s1 si1 g1 t1 i1 ac1))(AP n2 (Ap s2 si2 g2 t2 i2 ac2))=
    let name = ('(':n1)++('U':n2++")")
        s = name:(ns1 ++ ns2)
        si =nub (si1++si2)
        g = nub (g1++g2)
        ac = nac1 ++ nac2
        t =  (T name '/' "/" "/" (n1++i1)):(T name '/' "/" "/" (n2++i2)):nt1++nt2
    in AP name (Ap s si g t name ac) 
     where 
      ns1  = newStates s1 n1
      ns2  = newStates s2 n2
      nac1 = newStates ac1 n1
      nac2 = newStates ac2 n2
      nt1  = newTrans t1 n1 
      nt2  = newTrans t2 n2   

{-Crea un autómata de pila que acepta el lenguaje L1L2  , 
  donde L1 y L2 son los lenhuajes aceptados por los autómatas argumento-}
concatOp:: AP->AP-> AP
concatOp a (AP _ (Ap s1 si1 g1 t1 i1 ac1)) = 
  let (AP _ (Ap s2 si2 g2 t2 i2 ac2)) = emptyStack a
      name = ("1 C 2")
      s = (newStates s2 "2")++(newStates s1 "1") 
      si =nub (si1++si2)
      g = nub (g1++g2)
      ac = (newStates ac1 "1") 
      t = [(T ("2"++x) '/' "/" "/" ("1"++i1))| x <-ac2]++
           ((newTrans t1 "1")++
            (newTrans t2 "2" ))
  in AP name (Ap s si g t ("2"++i2) ac) 
    
---------------------------------------------------------------------------------



------------------------ <Función de ejecución > --------------------------------
type Trace = [(String, String,[String])]

{-Realiza la ejecucion de un AP sobre una cadena-}
execute:: Ap -> String -> Int-> Estado
execute ap@(Ap _ si _ _ i _) word cant = 
  if and $ map (\x->elem x si) word
  then execute' ap word [] i cant [(i,word,[])]
  else Error "Caracteres de la cadena no disponibles en el autómata"

------------------------ <<Funciones adicionales de la función de ejecución>> -----------------------------

{-Realiza la ejecucion de un AP sobre una cadena-}
execute':: Ap -> String -> [String] -> String -> Int -> Trace -> Estado
execute' _ _ _ _ 0 _ = Error "Limite de ciclos excedidó"
execute' a@(Ap _ _ _ t _ ac) word ps state i trace| elem state ac && null word = Okay
                                                       | otherwise = case filter (\z -> if word== "" then transaction '/' ps state z else transaction (head word) ps state z) t of
                                                                      [] ->  Error "No hay transiciones posibles"
                                                                      ts -> if hayBucle trace
                                                                            then Error "Ciclo autosuficiente detectado"
                                                                            else  paths ts word ps state a i trace
                                                                     
                                                            
{-Verifica si hay un bucle-}                                                                           
hayBucle:: Trace -> Bool
hayBucle [] = error "Error"
hayBucle [_] = False
hayBucle ((s,w,p) :xs) = elem (s,w,p) xs 

{-Verifica si una transicion es posible dada la pila , el estado actual y el simbolo a leer-}
transaction:: Char-> [String] ->String->Trans ->Bool
transaction l ps s (T a b c _ _) = and [a==s,(l==b||b=='/'),(stackVer ps c)]  
  
{-Verifica si el elemento a sacar de la pila esta disponible en la misma-}  
stackVer::[String] -> String -> Bool
stackVer [] c|c=="/" = True
             |otherwise = False 
stackVer (p:_) c|c==p = True
                |c == "/" = True
                |otherwise = False 

{-Realiza todas las transiciones posibles-}
paths:: [Trans]->String->[String]->String->Ap -> Int->Trace-> Estado
paths [] _ _ _ _ _ _= Error "Se agotaron las transiciones"
paths (t:ts) word ps s a i trace = 
  case chose t word ps a i trace of
    Error _ -> paths ts word ps s a i trace
    Okay -> Okay

{-Realiza la transicion elegida-}
chose::Trans->String->[String]->Ap->Int->Trace-> Estado  
chose (T _ b c d e) word ps ap@(Ap _ _ _ _ _ ac) i trace= 
 case (b,c) of
    ('/',"/") -> execute' ap word (change ps) e i' ((e, word,(change ps)):trace)
    (_,"/") ->  execute' ap (tail word) (change ps) e i' ((e, (tail word),(change ps)):trace)
    ('/',_) ->  execute' ap word (change (tail ps)) e i' ((e, word,(change (tail ps))):trace)
    (_,_) ->  execute' ap (tail word) (change (tail ps)) e i' ((e, (tail word),(change (tail ps))):trace)
    where change ps' = if d=="/" then ps' else d:ps'
          i' = i-1


---------------------------------------------------------------------------------

------------------------ <Función de operación> --------------------------------

{-Evalua una expresion y reajusta el AP resultante-}
oper ::Expresiones-> NameEnv Ap -> Maybe AP
oper a m = case operate a m of
             Just (AP name ap)-> Just (AP name (adjustStates ap))
             Nothing -> Nothing

------------------------ <<Funciones adicionales de la función de operación>> -----------------------------

{-Evalua una expresion-}
operate :: Expresiones -> NameEnv Ap -> Maybe AP
operate (Auto name) state = 
  case find (\(n,_)-> n==name) state of
    Just (_,ap) -> Just (AP name ap)
    Nothing -> Nothing 
operate (OpUnion a b) state = 
  case operate a state of 
   Just (AP n ap) -> case operate b state of
                      Just (AP n2 ap2) -> let (AP _ ap3) = (unionOp (AP n ap) (AP n2 ap2)) 
                                          in Just (AP ("(" ++ n ++" Union " ++ n2 ++")") ap3)
                      Nothing -> Nothing
   Nothing -> Nothing
operate (OpConcat a b) state = 
  case operate a state of 
   Just (AP n ap) -> case operate b state of
                      Just (AP n2 ap2) -> let (AP _ ap3)= (concatOp (AP n ap) (AP n2 ap2)) 
                                          in Just (AP ("(Concat " ++ n ++" " ++ n2 ++")") ap3)
                      Nothing -> Nothing
   Nothing -> Nothing

operate (OpKs a ) state =  
  case operate a state  of
    Just (AP n ap) -> let (AP _ ap2) = (kleeneStarOp (AP n ap)) 
                      in Just (AP ('*':n) ap2 )
    Nothing -> Nothing 
operate (OpKp a ) state =  
  case operate a state  of
   Just (AP n ap) -> let (AP _ ap2) = (kleenePlusOp (AP n ap)) 
                     in Just (AP ('+':n) ap2 )
   Nothing -> Nothing 
operate (OpPot a i) state =  
  case operate a state  of
   Just (AP n ap) -> let (AP _ ap2) = (potencyOp (AP n ap) i) 
                     in Just (AP ("(Potency " ++n++" "++(show i)++")") ap2 )
   Nothing -> Nothing 
operate (OpReverse a ) state =  
  case operate a state  of
    Just (AP n ap) -> let (AP _ ap2) = (reverseOpAp (AP n ap)) 
                      in Just (AP ("Reverse"++n) ap2 )
    Nothing -> Nothing 

---------------------------------------------------------------------------------

------------------------ <Funciones adcionales> ---------------------------------
{-Genera nuevas transiciones para un AP-}
newTrans :: [Trans]-> String -> [Trans]
newTrans t n = map (\(T a b c d e) -> (T (n++a) b c d (n++e))) t

{-Genera nuevos estados para un AP-}
newStates :: [String]-> String -> [String]
newStates s n= (map (\x -> n++x) s) 

{-Modifica un AP para que este vacie su pila-}
emptyStack::AP -> AP 
emptyStack (AP n (Ap s si g t i ac)) =
  let newS = ["S","P","Q"]++(map (\z -> 'E':z) s )
      newG = "#": g
      newAc =["Q"]
      newT = (T "S" '/' "/" "#" ('E':i) ):(T "P" '/' "#" "/" "Q" ):((newTrans t "E")++[(T ('E':p) '/' "/" "/" "P" )| p <- ac] ++ [(T "P" '/' p "/" "P" )| p <- g ])
  in (AP ('E':n) (Ap newS si newG newT "S" newAc))  


------------------------ <Función de ajustado de estados de un AP> ---------------------------------
{-Renombra los estados de un AP-}
adjustStates:: Ap -> Ap  
adjustStates (Ap s si g t i ac) =
  adjustS (Ap s1 si g t1 i1 ac1 ) (createCad s)
  where 
    s1 = newStates s "¬"
    ac1 = (newStates ac "¬") 
    t1 = (newTrans t  "¬")
    i1= '¬': i


{-Crea permutaciones de los 
  elementos de abc segun como sea 's'-}
createCad:: [String]-> [String]
createCad s = createPerms $cantPerms (length s) (length abc)(length abc) 1
         where cantPerms s n m ca| s <= n = ca
                                 | otherwise = cantPerms s (n*m) m (ca+1)
               createPerms 1 = [a:[]|a<-abc]
               createPerms n = [a:b |a<-abc , b<- createPerms (n-1)]

{-Renombra los estados de un AP-}
adjustS:: Ap -> [String]-> Ap
adjustS _ [] = error "La lista es vacia"
adjustS a@(Ap [] si g t  i ac) names= a
adjustS a@(Ap (x:xs) si g t i ac) names =
   let 
      newS = head names
      newI = if x == i then newS else i
      newT = map (replaceS x newS) t
      newAc = map (\z-> if x == z then newS else z) ac 
      (Ap ps2 si2 g2 t2 i2 ac2) = adjustS (Ap xs si g newT newI newAc) (tail names)
  in (Ap (newS:ps2) si2 g2 t2 i2 ac2)


{-Reemplaza los nuevos nombres de los estados en una transicion-}
replaceS:: String ->String -> Trans ->Trans
replaceS x s (T a b c d e)| x == a && x /=e = (T s b c d e)
                          | x /= a && x /=e = (T a b c d e)
                          | x /= a && x ==e = (T a b c d s)
                          | x == a && x ==e = (T s b c d s )

abc::String
abc ="ABCDEFGHIJKLMNOPQRSTUVWXYZ"

---------------------------------------------------------------------------------




------------------------ <Función de conversion de GI a AP> ---------------------------------
{-Combierte una gramática independiente del contexto en un autómata de pila-}
grammarToAp:: Gi -> Ap 
grammarToAp (Gi n t p i) =
  let rTt = rulesToTranss p 1 
      newSi = map head t 
      newG = "@":(t++n)
      newS =["p","q","f","i"]++[show x | x <- [1..((length rTt) -length p)]]
      newT =[T "i" '/' "/" "@" "p" , 
             T "p" '/' "/" i "q" ,
             T "q" '/' "@" "/" "f" 
             ]++rTt++ [T "q" (head x) x "/" "q"| x<-t] 
      ap = (Ap newS newSi newG newT "i" ["f"])

  in ap           

{-Genera las transiciones del AP correspondientes a un regla de la GI-}
ruleToTrans::[String] -> Int-> String  -> [Trans]
ruleToTrans [] _ _  = [T "q" '/' "/" "/" "q"]
ruleToTrans [x] i q = [T q '/' "/" x "q"]
ruleToTrans (x:xs) i q = (T q '/' "/" x (show i)) : ruleToTrans xs (i+1) (show i) 

{-Genera las transiciones del AP a traves de las reglas de la GI-}
rulesToTranss:: [Rules] -> Int -> [Trans]
rulesToTranss [] _ = error "Error"
rulesToTranss [(R x xs)] n = add (ruleToTrans xs n "q") x
rulesToTranss ((R x xs):rs) n = 
  let list=ruleToTrans xs n "q" 
      recur = rulesToTranss rs (n+((length list) - 1 )) 
  in (add list x)++recur

{-Termina con la creacion de las transiciones del AP-}
add :: [Trans] -> String -> [Trans] 
add [] _ = error "Error"
add [(T a b c d e)] x= [(T a b x d e)]
add ((T a b c d e):xs) x= (T a b x d e):xs

---------------------------------------------------------------------------------



------------------------ <Función de conversion de AP a GI> ---------------------------------
{-Combierte un autómata de pila en gramática independiente del contexto-}
apToGrammar::Ap -> Gi
apToGrammar a@(Ap s si g t i ac)=
 let
    newT = map (\x -> x : []) si 
    newR = transsToRules a
    g'="/":g
    newN = "S":["<"++ p ++ x ++q++">"| p<-s,q<-s,x<-g']
 in (Gi newN newT newR "S")


{-Crea las reglas de la GI a partir del AP-}
transsToRules:: Ap -> [Rules]
transsToRules (Ap s si g t i ac) = 
  [R "S" ["<"++ i ++ "/" ++f++">"]| f <- ac]++sndStep s++trdStep t s++frdStep t s g

{-Segundo paso de la conversion de AP a GI-}
sndStep:: [String] -> [Rules]
sndStep xs = [R ("<"++ p ++ "/" ++p++">") [] | p <- xs]

{-Tercer paso de la conversion de AP a GI-}
trdStep:: [Trans]->[String] -> [Rules]
trdStep xs ys = [R ("<"++ p ++ y ++r++">")[x:[],"<"++ q ++ z ++r++">"]|(T p x y z q)<-xs , y /= "/",r<- ys]

{-Cuarto paso de la conversion de AP a GI-}
frdStep:: [Trans]->[String] ->[String]-> [Rules]
frdStep xs ys gs = 
  let gs'="/":gs
  in [R ("<"++ p ++ w ++r++">")[x:[],"<"++ q ++ z ++k++">","<"++ k ++ w ++r++">"]|(T p x y z q)<-xs , y == "/",r<- ys,k<- ys,w<-gs']

---------------------------------------------------------------------------------




------------------------ <Función de ajustado de simbolos de pila de un AP> ---------------------------------
{-Renombra los os simbolos de pila de un AP-}
adjustG:: Ap -> [String]-> Ap
adjustG _ [] = error "La lista es vacia"
adjustG a@(Ap s si [] t i ac) names= a
adjustG a@(Ap s si (x:xs) t i ac) names =
   let 
      newG = head names
      newT = map (replaceG x newG) t
      (Ap ps2 si2 g2 t2 i2 ac2) = adjustG (Ap s si xs newT i ac) (tail names)
  in (Ap ps2 si2 (newG:g2) t2 i2 ac2)


{-Reemplaza los nuevos nombres de los los simbolos de pila en una transicion-}
replaceG:: String ->String -> Trans ->Trans
replaceG x s (T a b c d e)| x == c && x /=d = (T a b s d e)
                          | x /= c && x /=d = (T a b c d e)
                          | x /= c && x ==d = (T a b c s e)
                          | x == c && x ==d = (T a b s s a)

{-Renombra los simbolos de pila de un AP-}
adjustGamma :: Ap -> Ap  
adjustGamma (Ap s si g t i ac) = 
  adjustG (Ap s si g t i ac) (createCad (s++[show a |a<- [1..((length abc)+1)]]))
---------------------------------------------------------------------------------


---------------------------------------------------------------------------------

