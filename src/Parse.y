{
module Parse where
import Data.Char
import Data.List
import Common
}

%monad { P } { thenP } { returnP }
%name buildUpAPs MultipleComm
%name buildUpAP SingleComm

%tokentype { Token }
%lexer { lexer } { TEOF }


%token
      SYMBOLU         { TokenSymU $$ }
      SYMBOLL         { TokenSymL $$ }
      STRING           { TokenSTRING $$ }
      NAT             { TokenNAT $$}
      DEFA             { TokenDefA }
      DEFG             { TokenDefG }
      DEFE             { TokenDefE }
      ';'             { TokenT }
      '|'             { TokenDiv}
      '/'             { TokenBar}
      '_'             { TokenDown}
      '->'            { TokenArrow }
      '-'             { TokenMinus }
      ','             { TokenComma }
      '='             { TokenEqual}
      '('             { TokenOB }
      ')'             { TokenCB }
      UNION           { TokenU }
      KS              { TokenKs }
      KP              { TokenKp }
      POT             { TokenPot } 
      REVERSE         { TokenReverse }
      CONCAT          { TokenConc }
      ZERO            { TokenZero}



%left UNION
%left CONCAT POT
%right KS KP REVERSE

%%


MultipleComm        : SingleComm  MultipleComm     {$1 : $2}
                    |                                      {[]}

SingleComm          : CommExp                           {$1}
                    | CommDef                           {$1}
                    
{-Comcandos -}

CommDef        : DEFA Cadena '=' AP ';'               {DefAp (AP $2 $4) }
	        | DEFE Cadena '=' Exp ';'     	 {DefExp $2 $4 }
	        | DEFG Cadena '=' GI ';'               {DefG (GI $2 $4)}


CommExp     : Exp CadenaV NaturalsZ ';'                 {ApEval $1 $2 $3}
               | Exp CadenaV ';'                        {ApEval $1 $2 4096}
               | Exp ';'                                {ApExp $1}
               
{-Gramatica independiente del contexto-}               
GI             : N '|' T '|' TransGs '|' Cadena        { Gi $1 $3 $5 $7}

N              : N ',' CadenaNonTerm                         {$3 : $1 }
               |CadenaNonTerm                                { [$1] }

T              : T ',' SYMBOLL                         {[$3] : $1 }
               |SYMBOLL                                { [[$1]] }
   
TransGs        :TransGs '-' TransG                     {$3 : $1 }
               |TransG                                 { [$1] }
        
TransG         : Cadena '->' CadenasGra                {R $1 $3}   	

   	

   
{-Autòmata de pila-}    
   
AP    : S '|' Sigma '|' Gamma '|' Transs '|' IS '|' Ac { Ap $1 $3 $5 $7 $9 $11}

S     : S ',' Cadena           {$3 : $1 }
      |Cadena                  { [$1] }

Sigma : Sigma ',' Symbol       {$3 : $1 }
      |Symbol                  { [$1] }

Gamma :Gamma ',' CadenaPila      {$3 : $1 }
      |CadenaPila                 { [$1] }
      |                           { [] }

Transs :Transs '-'Trans    {$3 : $1 }
       |Trans              { [$1] }
       |                   { [] }

Trans : '(' Cadena ',' SymbolV ',' CadenaPila ',' CadenaPila',' Cadena ')' {T $2 $4 $6 $8 $10}

IS    :Cadena                   {$1}

Ac     :Ac ',' Cadena           {$3 : $1 }
      |Cadena                   { [$1] }
 
 
 
{-Expresiones-} 
Exp           : Exp UNION Exp                          {OpUnion $1 $3} 
	      | KS Exp                                 {OpKs $2}
	      | KP Exp                                 {OpKp $2}
	      | CONCAT Exp Exp                         {OpConcat $2 $3}
	      | POT Exp NAT                            {OpPot $2 $3}
	      | REVERSE Exp                            {OpReverse $2}
	      | Ap                      {$1}

Ap : '(' Exp ')'              {$2}
   |Cadena                    {Auto $1} 
 
 
 
 
{-Cadenas y simbolos disponiles-} 
 
Symbol : SYMBOLU                         {$1} 
        | SYMBOLL                        {$1} 

SymbolV : '/'                            {'/'}   
        | Symbol                         {$1} 
    
CadenaPila: Cadena                       {$1}
          |  '/'                         {"/"} 
          
CadenasGra :'_'                          {[]} 
           | ListaCadena                 {$1}     
   
CadenaV: '_'                            {""} 
       | Cadena                         {$1}

Cadena : STRING                         {$1}
       | Symbol	                 {[$1]}
 
 
 
CadenaNonTerm: STRING                         {$1}
              |SYMBOLU                        {[$1]} 
  
ListaCadena : ListaCadena ',' Cadena           {$3 : $1 }
            |Cadena                            { [$1] }
            
NaturalsZ : ZERO         {0}  
          | NAT          {$1}          
          
                                 
{
---VER EL TEMA DE LA PRECEDENCIA QUE CREO QUE ES AL PEDO


data ParseResult a = Ok a | Failed String  deriving Show

type LineNumber = Int

type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e

returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l
                        
--parseError :: Token -> P a
--parseError t = getLineNo `thenP` \line -> failP ("Línea "++ show line ++ ": parse error" ) 

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)  --no se que onda con esto ajaj

data Token
      = TokenSymU Char
      | TokenSymL Char 
      | TokenSTRING String
      | TokenNAT Int
      | TokenPlus
      | TokenMinus
      | TokenComma 
      | TokenOB
      | TokenCB
      | TokenDefA 
      | TokenDefE 
      | TokenDefG 
      | TokenEqual
      | TokenU
      | TokenKs
      | TokenKp
      | TokenReverse
      | TokenDiv
      | TokenConc
      | TEOF
      | TokenArrow
      | TokenT
      | TokenBar
      | TokenPot 
      | TokenDown
      | TokenZero
      
 deriving Show


lexer :: (Token -> P a) -> P a
lexer cont s = case s of
		[] -> cont TEOF []
		('\n':s)  ->  \line -> lexer cont s (line + 1)
		(c:cs)
      		      | isSpace c -> lexer cont cs
      		      | isAlpha c -> lexS (c:cs)
      		      | isDigit c -> lexD (c:cs)
      		('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
      		('{':('-':cs)) -> comentarios 0 0 cont cs
      		('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
      		('(':cs) -> cont TokenOB cs
      		(')':cs) -> cont TokenCB cs
      		( ',':cs) -> cont TokenComma cs
      		( '-':('>':cs)) -> cont TokenArrow cs
      		( '-':cs) -> cont TokenMinus cs
      		( '*':cs) -> cont TokenKs cs
      		( ';':cs) -> cont TokenT cs
      		( '_':cs) -> cont TokenDown cs
      		( '/':cs) -> cont TokenBar cs
      		( '+':cs) -> cont TokenKp cs
      		( '|':cs) -> cont TokenDiv cs
      		('=':cs) -> cont TokenEqual cs
      		unknown -> \line -> Failed $
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..." 
               where lexD cs = let (number,rest) = span isDigit cs
                               in if (read number :: Int) == 0
                               then cont TokenZero rest
                               else cont (TokenNAT (read number :: Int )) rest
                     lexS cs = case span isAlpha cs of
                                ("defA",rest)     -> cont TokenDefA rest
                                ("defE",rest)     -> cont TokenDefE rest
                                ("defG",rest)     -> cont TokenDefG rest
                                ("Union",rest)     -> cont TokenU rest   
                                ("Potency",rest)     -> cont TokenPot rest
                                ("Concat",rest)     -> cont TokenConc rest
                                ("Reverse",rest)     -> cont TokenReverse rest
                                (something,rest) -> if (length something) > 1 
                                                     then cont (TokenSTRING something) rest
                                                     else if isUpper (head something)
                                                          then cont (TokenSymU (head something)) rest
                                                          else cont (TokenSymL (head something)) rest
                     comentarios anidado cl cont s = case s of
                                ('-':('-':cs)) -> comentarios anidado cl cont $ dropWhile ((/=) '\n') cs
                                ('{':('-':cs)) -> comentarios (anidado+1) cl cont cs
                                ('-':('}':cs)) -> case anidado of
                                                  0 -> \line -> lexer cont cs (line+cl)
                                                  _ -> comentarios (anidado-1) cl cont cs
                                ('\n':cs) -> comentarios anidado (cl+1) cont cs
                                (_:cs) -> comentarios anidado cl cont cs

		   
buildUpAPs' s = buildUpAPs s 1
buildUpAP' s = buildUpAP s 1
} 


