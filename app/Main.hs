module Main ( main ) where

import Control.Concurrent ( threadDelay )
import System.Environment ( getArgs )
import System.Exit ( ExitCode (..) )
import System.IO.Error ( mkIOError, doesNotExistErrorType)
import Shell ( tmuxSafeQueryEnvVariable, moshStartServerGrabResponse )
import Tui ( brick )
import Pipes ( pipesMain )

printHelpDialog :: IO ()
printHelpDialog = putStr $ unlines
    [ "doormat: A Brick-based ncurses ssh greeter and tmux menu"
    , ""
    , "This program does two things:"
    , "     - \"doormat\" will open the greeter TUI."
    , "     - \"doormat --lookup VAR\" will query the TMUX local environment"
    , "          for env variable VAR and prints its value."
    , "     - \"doormat --mosh\" will start a mosh-server before attaching."
    , "          This is useful when connecting remotely."
    , "     - \"doormat --help\" displays this dialog.\""
    ]

lookupVar :: String -> IO String
lookupVar var = do
    maybe_val <- tmuxSafeQueryEnvVariable var
    case maybe_val of
        Just x -> return x
        Nothing -> ioError $ mkIOError doesNotExistErrorType var Nothing Nothing

conditionalShow :: String -> Maybe String -> String
conditionalShow _ Nothing    = mempty
conditionalShow s1 (Just s2) = s1 <> s2  

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> brick
        ["--help"] -> printHelpDialog
        ["--lookup", var] -> putStr =<< lookupVar var
        ["--pipes"] -> pipesMain
        ["--mosh"] -> do
          (errcode, msg) <- moshStartServerGrabResponse
          case errcode of
            ExitSuccess   -> case msg of
              Just m  -> putStr m >> threadDelay 1000000 >> brick --wait a second
              Nothing -> ioError $ userError "mosh server did not echo start phrase."
            ExitFailure e -> do
              let emsg = "mosh server failed to start. Returned code " <> show e <> "." <> conditionalShow "\n" msg
              ioError $ userError emsg
        _invalidCmd -> ioError $ userError $ "Invalid command \"" <> unwords args <> "\". Use flag --help for help."
