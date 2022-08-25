module Main where

import Shell

nicePutStrLn :: String -> IO ()
nicePutStrLn s = putStrLn $ "(shell-test) >>= " <> s

testSessionName = "SHELL-TEST"

sessionCheck :: IO Bool
sessionCheck = do
    nicePutStrLn $ "Checking if " <> testSessionName <> " already exists"
    has_session <- tmuxHasSession testSessionName
    if has_session
        then nicePutStrLn $ testSessionName <> " exists!"
        else nicePutStrLn $ testSessionName <> " does not exist!"
    return has_session

main :: IO ()
main = do
    nicePutStrLn "Running doormat shell-test."
    nicePutStrLn "Listing tmux sessions:"
    print =<< tmuxGetSessions
    sessionCheck >>= ( \ x ->
        if x
            then nicePutStrLn "Found existing test session. Killing it." 
                >> killTmuxSession testSessionName
            else nicePutStrLn "Creating tmux session."
                >> makeTmuxSession testSessionName )
    nicePutStrLn $ "Tmux should report a session \"" <> testSessionName <> "\" now."
    print =<< tmuxGetSessions
    sessionCheck >>= ( \ x ->
        if x
            then killTmuxSession testSessionName
            else nicePutStrLn "The test session is missing! (What happened?)" )
    nicePutStrLn
        "Now doing an env lookup test on TMUX. If not in TMUX session this should fallback to lookupEnv and return Nothing."
    print =<< tmuxSafeQueryEnvVariable "TMUX"
    nicePutStrLn
        "Now doing an env lookup test on PATH. This should always return something. (Why would PATH not be set?)"
    print =<< tmuxSafeQueryEnvVariable "PATH"
    nicePutStrLn
        "Now doing an env lookup on TESTVAR. Try running this in a TMUX session which has TESVAR in the local environment."
    print =<< tmuxSafeQueryEnvVariable "TESTVAR"
    nicePutStrLn "Test complete!"
