module Main where

import           Control.Exception              ( catch
                                                , IOException
                                              )
import           Control.Monad.Except
import           Data.Char
import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( print )
import           System.Console.Haskeline
import qualified Control.Monad.Catch           as MC
import           System.Environment
import           System.IO               hiding ( print )
import           Text.PrettyPrint.HughesPJ      ( render
                                                )

import           Printer
import           Common
import           Parse
import           ApFunctions
import           Security
---------------------
--- Interpreter
---------------------

data Command = Compile CompileForm
              | Print String
              | Recompile
              | Browse
              | Quit
              | Help
              | Noop

data CompileForm = CompileInteractive  String
                  | CompileFile         String 

data InteractiveCommand = Cmd [String] String (String -> Command) String

data State = S
  { inter :: Bool
  ,       -- True, si estamos en modo interactivo.
    lfile :: String
  ,     -- Ultimo archivo cargado (para hacer "reload")
    ve    :: NameEnv Ap-- Entorno para autómatas [(Name, Ap)]
  }


main :: IO ()
main = runInputT defaultSettings main'

main' :: InputT IO ()
main' = do
  args <- lift getArgs
  readevalprint args (S True "" [])

iname, iprompt :: String
iname = "Simulador de Autómatas de pila"
iprompt = "AP>> "

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing


readevalprint :: [String] -> State -> InputT IO ()
readevalprint args state@(S int _ _) =
  let rec st = do
        mx <- MC.catch  
          (if int then getInputLine iprompt else lift $ fmap Just getLine) 
          (lift . ioExceptionCatcher) 
        case mx of
          Nothing -> return ()
          Just "" -> rec st
          Just x  -> do
            c   <- interpretCommand x
            st' <- handleCommand st c
            maybe (return ()) rec st'
  in  do
        state' <- compileFiles args state
        when int $ lift $ putStrLn 
          (  "Intérprete del "
          ++ iname
          ++ ".\n"
          ++ "Escriba :? para recibir ayuda."
          )
        --  enter loop
        rec state' { inter = True }

interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $ if isPrefixOf ":" x
  then do
    let (cmd, t') = break isSpace x
    let t         = dropWhile isSpace t'
    --  find matching commands
    let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
    case matching of
      [] -> do
        putStrLn
          ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda."
          )
        return Noop
      [Cmd _ _ f _] -> do
        return (f t)
      _ -> do
        putStrLn
          (  "Comando ambigüo, podría ser "
          ++ concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ])
          ++ "."
          )
        return Noop
  else return (Compile (CompileInteractive x))



commands :: [InteractiveCommand]
commands =
  [ Cmd [":browse"] "" (const Browse) "Ver los nombres en scope"
  , Cmd [":load"]
        "<file>"
        (Compile . CompileFile)
        "Cargar un programa desde un archivo"
  , Cmd [":print"] "<ap>" Print "Imprime un AP y su AST"
  , Cmd [":reload"]
        "<file>"
        (const Recompile)
        "Volver a cargar el último archivo"
  , Cmd [":quit"]       ""       (const Quit) "Salir del intérprete"
  , Cmd [":help", ":?"] ""       (const Help) "Mostrar esta lista de comandos"
  ]


handleCommand :: State -> Command -> InputT IO (Maybe State)
handleCommand state@(S int file v) cmd = case cmd of
  Quit   -> lift $ when (not int) (putStrLn "!@#$^&*") >> return Nothing 
  Noop   -> return (Just state)
  Help   -> lift $ putStr (helpTxt commands) >> return (Just state)
  Browse -> lift $ do
    putStr (unlines (reverse (nub (map fst v)))) 
    return (Just state)
  Compile c -> do
      state' <- case c of
          CompileInteractive s -> compilePhrase state s
          CompileFile        f -> compileFile (state { lfile = f }) f
      return (Just state')
  Print s ->
   let s' = reverse (dropWhile isSpace (reverse (dropWhile isSpace s)))
   in  searchA s' state >> return (Just state)
  Recompile -> if null file
    then lift $ putStrLn "No hay un archivo cargado.\n" >> return (Just state)
    else handleCommand state (Compile (CompileFile file))

compilePhrase :: State -> String -> InputT IO State
compilePhrase state x = do x' <- parseIO "<interactive>" buildUpAP' x
                           maybe (return state) (handleAPs state) x'


searchA::String -> State -> InputT IO ()
searchA name (S _ _ v) = 
  let a = find (\(n,_)-> n==name) v 
  in case a of  
     Just (_,n) ->lift (printAp name n)
     Nothing -> lift $ putStrLn ("No se ha encontrado el autómata " ++ name)

printAp ::String -> Ap-> IO () 
printAp name a = putStrLn outtext
  where outtext = 
            render (printAP a name)
            ++ "\n\nY su AST:\n"
            ++ show a 
            ++ "\n\n"  

helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n"
    ++ "c es el primer caracter del nombre completo.\n\n"
    ++ unlines
         (map
           (\(Cmd c a _ d) ->
             let
               ct =
                 concat
                   (intersperse ", "
                                (map (++ if null a then "" else " " ++ a) c)
                   )
             in  ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d
           )
           cs
         )

compileFiles :: [String] -> State -> InputT IO State
compileFiles xs s = foldM (\z x -> compileFile (z { lfile = x, inter = False }) x) s xs   

compileFile :: State -> String -> InputT IO State
compileFile state@(S _inter _lfile _ve) f = do
  lift $ putStrLn ("Abriendo " ++ f ++ "...")
  let f' = reverse (dropWhile isSpace (reverse f))
  x <- lift $ Control.Exception.catch
    (readFile f')
    (\e -> do
      let err = show (e :: IOException)
      hPutStr stderr
              ("No se pudo abrir el archivo " ++ f' ++ ": " ++ err)
      return ""
    )
  aps <- parseIO f' (buildUpAPs') x 
  maybe (return state) (foldM handleAPs state) aps 

parseIO :: String -> (String -> ParseResult a) -> String -> InputT IO (Maybe a)
parseIO f p x = lift $ case p x of
  Failed e -> do
    putStrLn (f ++ ": " ++ e)
    return Nothing
  Ok r -> return (Just r)   


handleAPs :: State -> Comms -> InputT IO State
handleAPs state (DefAp (AP name a)) = 
  case apChecker a of 
   Error e -> lift $ do putStrLn ("No se ha podidó cargar " ++ name ++ " -ERROR: " ++ e)
                        return state
   Okay-> let ap' = apReconstructor a 
          in lift $ (putStrLn name) >> return (state { ve = (name, ap') : ve state })
handleAPs state@(S _ _ v) (DefExp name ex) = 
  case oper ex v of
   Nothing -> lift $ do putStrLn ("No se ha podidó cargar " ++ name ++ " -ERROR: Autómatas de la expresión no encontrados")
                        return state
   Just (AP _ ap1)-> lift $ (putStrLn name) >> return (state { ve = (name, ap1) : ve state }) 
handleAPs state (DefG (GI name g)) = 
  case giChecker g of 
    Error e -> lift $ do putStrLn ("No se ha podidó cargar " ++ name ++ " -ERROR: " ++ e)
                         return state
    Okay-> let gi = giReconstructor g 
           in lift $ (putStrLn name) >> return (state { ve = (name, (adjustStates $ grammarToAp gi)) : ve state })
handleAPs state@(S _ _ v) (ApExp ex)=
  case oper ex v of
   Nothing -> lift $ do putStrLn ("No se ha podidó cargar el autómata -ERROR: Autómatas de la expresión no encontrados")
                        return state
   Just (AP n ap1)->lift $ do printAp n ap1
                              return state
handleAPs state@(S _ _ v) (ApEval op w n)=
  case oper op v of
    Nothing -> lift $ do putStrLn ("No se ha podidó cargar el autómata -ERROR: Autómatas de la expresión no encontrados")
                         return state
    Just (AP name ap1)->case execute ap1 w n of 
                      Okay -> lift $ putStrLn ("La cadena <"++w++"> fue aceptada por el autómata " ++ name) >> return state
                      Error _ -> lift $ putStrLn ("La cadena <"++w++"> NO fue aceptada por el autómata " ++ name) >> return state
