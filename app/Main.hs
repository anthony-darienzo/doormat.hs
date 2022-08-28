module Main ( main ) where

import System.Environment ( getArgs )
import System.IO.Error ( mkIOError, doesNotExistErrorType)
import Shell ( tmuxSafeQueryEnvVariable )
import Tui ( brick )

printHelpDialog :: IO ()
printHelpDialog = putStr $ unlines
    [ "doormat: A Brick-based ncurses ssh greeter and tmux menu"
    , ""
    , "This program does two things:"
    , "     - \"doormat\" will open the greeter TUI."
    , "     - \"doormat --lookup VAR\" will query the TMUX local environment"
    , "          for env variable VAR and prints its value."
    , "     - \"doormat --help\" displays this dialog.\""
    ]

lookupVar :: String -> IO String
lookupVar var = do
    maybe_val <- tmuxSafeQueryEnvVariable var
    case maybe_val of
        Just x -> return x
        Nothing -> ioError $ mkIOError doesNotExistErrorType var Nothing Nothing

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> brick
        ["--help"] -> printHelpDialog
        ["--lookup", var] -> putStr =<< lookupVar var
        _ -> ioError $ userError $ "Invalid command \"" <> unwords args <> "\". Use flag --help for help."