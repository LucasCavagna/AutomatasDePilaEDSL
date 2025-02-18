module Common where

  --Entorno 
  type NameEnv v = [(String, v)]

  --Comandos 
  data Comms =   DefAp AP                    
               | DefExp String Expresiones
               | ApEval Expresiones String Int
               | ApExp Expresiones 
               | DefG GI
               deriving Show 
  
  --AST de un autómata de pila
  data AP = AP String Ap deriving Show              

  data Ap = Ap [String] [Char] [String] [Trans] String [String]  deriving Show 
  
  data Trans = T String Char String String String deriving (Show,Eq)
  
  --AST de una gramatica independiente del contexto
  data GI = GI String Gi deriving Show 

  data Gi = Gi [String] [String] [Rules] String  deriving Show    

  data Rules = R String [String] deriving (Show,Eq)
  
  --Expresiones
  data Expresiones = Auto String 
                   | OpUnion Expresiones Expresiones
                   | OpConcat Expresiones Expresiones
                   | OpKs Expresiones
                   | OpKp Expresiones
                   | OpPot Expresiones Int
                   | OpReverse Expresiones deriving Show 


  --Estructura para devolver errores
  data Estado = Okay | Error String deriving Show




