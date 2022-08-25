module Main where

import Shell (tmuxGetSessions, enterTmuxSession)

nicePutStrLn :: String -> IO ()
nicePutStrLn s = putStrLn $ "(shell-test) >>= " <> s

main :: IO ()
main = do
    nicePutStrLn "Running doormat shell-test."
    nicePutStrLn "Listing tmux sessions:"
    sessionList <- tmuxGetSessions
    print sessionList
    nicePutStrLn "Creating/Entering tmux session SHELL-TEST"
    enterTmuxSession "SHELL-TEST"
