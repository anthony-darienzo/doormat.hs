module Shell (
        tmuxGetSessions
    ,   tmuxHasSession
    ,   enterTmuxSession
    ,   killTmuxSession
    ,   makeTmuxSession
    ,   enterCmd
    ,   reenterCmd
    ,   tmuxSafeQueryEnvVariable
) where

import System.Environment (lookupEnv)
import System.Process
import System.Posix.Process (executeFile)
import System.Exit ( ExitCode (..) )
import Data.List( intercalate )
import Control.Monad ( void, when )


---- COMMANDS AND ARGS
sh = "/bin/sh"
execFlag = "-c"

tmux = "tmux"
setEnvironment = "set-environment"
showEnvironment = "show-environment"
listSessions = "list-sessions"
newSession = "new-session"
hasSession = "has-session"
killSession = "kill-session"

sessionFlag = "-t"
envFlag = "-e"
sessionNameFlag = "-s"
attachFlag = "-A"
noAttachFlag = "-d"
formatFlag = "-F"
onlySessionName = "\"#{session_name}\""

lang = "en_US.UTF-8"
----

data Colormode = DARKMODE | LIGHTMODE | BLUEMODE

instance Show Colormode where
    show DARKMODE   = "1"
    show LIGHTMODE  = "-1"
    show BLUEMODE   = "0"

tmuxMaybeHandler:: [String] -> IO (Maybe String)
tmuxMaybeHandler args = do
    (ex, o, e) <- readProcessWithExitCode tmux args []
    if ex == ExitSuccess
        then return (Just o)
        else return Nothing

tmuxGetSessions :: IO [String]
tmuxGetSessions = do
    maybe_res <- tmuxMaybeHandler 
        [listSessions, formatFlag, onlySessionName]
    case maybe_res of
        Nothing     -> return []
        Just res    -> return . lines $ res 

tmuxHasSession :: String -> IO Bool
tmuxHasSession s = do
    (ex, o, e) <- readProcessWithExitCode tmux [hasSession, sessionFlag, s] []
    case ex of
        ExitSuccess     -> return True
        ExitFailure _   -> return False

getTmuxEnv :: Colormode -> [(String, String)]
getTmuxEnv c = [("DARKMODE", show c), ("LANG", lang)]

kvPairsToEFlags :: [(String, String)] -> String
kvPairsToEFlags kvs = unwords $ map ( \ (x,y) -> envFlag <> " " <> x <> "=" <> y ) kvs

kvPairToTmuxSet :: (String, String) -> String -> String
kvPairToTmuxSet (k,v) session_name = 
    unwords [ tmux, setEnvironment, sessionFlag, session_name, k, v ]

enterCmd :: String -> Colormode -> String
enterCmd session_name colormode =
    let envFlags = unwords [ (kvPairsToEFlags . getTmuxEnv) colormode ]
    in unwords [ tmux, newSession, envFlags, attachFlag, sessionNameFlag, session_name ]
    
reenterCmd :: String -> Colormode -> String
reenterCmd session_name colormode =
    let 
        tmuxSets = map (`kvPairToTmuxSet` session_name) (getTmuxEnv colormode)
        setCmd = intercalate "; " tmuxSets
        basicEnterCmd = unwords [ tmux, newSession, attachFlag, sessionNameFlag, session_name ]
    in unwords [ setCmd, ";", basicEnterCmd ]

{- | Uses executeFile to call exec(3) and enter tmux. This will not return to
   haskell! Furthermore, this will update DARKMODE and LANG env variables, at
   least in the TMUX local environment. One needs to check the local env
   accordingly. -}
enterTmuxSession :: String -> IO ()
enterTmuxSession session_name = do
    has_session <- tmuxHasSession session_name
    let cmd = if has_session
                then reenterCmd session_name DARKMODE
                else enterCmd session_name DARKMODE
    executeFile -- -> IO a
        sh -- Command
        True -- Search PATH?
        [ execFlag, cmd ] -- args
        Nothing -- Environment

{- | Make a TMUX session with specified name. If session already exists, do
   nothing. -}
makeTmuxSession :: String -> IO ()
makeTmuxSession session_name = do
    has_session <- tmuxHasSession session_name
    if has_session 
        then return ()
        else 
            let args = [ newSession, noAttachFlag, sessionNameFlag, session_name ]
            in void $ tmuxMaybeHandler args

killTmuxSession :: String -> IO ()
killTmuxSession session_name =
    let args = [ killSession, sessionNameFlag, session_name ]
    in tmuxHasSession session_name >>= flip when ( void $ tmuxMaybeHandler args)

{- | ( trim "NAME=VAL" NAME ) should reduce to Just VAL. Reduces to Nothing if
   something weird happens. -}
trim :: String -> String -> Maybe String
trim equation [] = 
    case equation of
        '=' : val -> Just val
        val -> Just val
trim [] _ = Nothing
trim (e : quation) (n : ame) =
    if e == n then trim quation ame else Nothing

dropLastLineBreak :: String -> String
dropLastLineBreak = helper []
    where
        helper acc [] = reverse acc
        helper acc [x] = 
            if x == '\n'
                then reverse acc
                else reverse (x:acc)
        helper acc (x:xs) = helper (x:acc) xs

{- | Helper command to query the value of an environment variable. Returns
   Nothing if lookup fails. If not in TMUX session, this will lookup the local
   env of the most recent session first! -}
tmuxSafeQueryEnvVariable :: String -> IO (Maybe String)
tmuxSafeQueryEnvVariable var = do
    (ex, o, e) <- readProcessWithExitCode tmux [showEnvironment, var] []
    case ex of
        -- we drop the last line break because o (stdout) will have an extra one.
        ExitSuccess     -> return $ fmap dropLastLineBreak ( trim o var )
        -- if var does not exist in tmux local env. Fallback to global env.
        ExitFailure _   -> lookupEnv var 
