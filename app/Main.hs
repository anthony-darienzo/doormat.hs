module Main where

import System.Environment ( getArgs )
import System.IO.Error ( mkIOError, doesNotExistErrorType)
import Shell
    ( tmuxSafeQueryEnvVariable,
      enterTmuxSession,
      tmuxGetSessions,
      Colormode(..), makeTmuxSession, killTmuxSession )

import Lens.Micro ((^.), Lens', lens)
import Lens.Micro.Mtl ( use, zoom )
import Control.Monad.State (modify, liftIO, unless)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import Brick.Util (fg, on)
import qualified Brick.Widgets.Edit as E
import Data.Vector.Generic.New (New(New))
import Data.Text.Zipper (currentLine, clearZipper)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> brick
        ["--help"] -> printHelpDialog
        ["--lookup", var] -> putStr =<< lookupVar var
        _ -> putStrLn $ "Invalid command \"" <> unwords args <> "\". Use flag --help for help."

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

-- Begin app code

data Name = SessionList | ColormodeList | NewSessionEditor deriving (Ord, Eq, Show)

data AppState = AppState
    { focusRing :: F.FocusRing Name
    , sessionList :: L.List Name String
    , colormodeList :: L.List Name Colormode
    , newSessionEditor :: E.Editor String Name
    , escapeToShell :: Bool
    , editSessionVisible :: Bool
    }

getFocusRing :: Lens' AppState (F.FocusRing Name)
getFocusRing = lens focusRing
    (\ s r -> AppState
        { focusRing = r
        , sessionList = sessionList s
        , colormodeList = colormodeList s
        , newSessionEditor = newSessionEditor s
        , escapeToShell = escapeToShell s
        , editSessionVisible = editSessionVisible s
        })

getSessionList :: Lens' AppState (L.List Name String)
getSessionList = lens sessionList
    (\ s l -> AppState
        { focusRing = focusRing s
        , sessionList = l
        , colormodeList = colormodeList s
        , newSessionEditor = newSessionEditor s
        , escapeToShell = escapeToShell s
        , editSessionVisible = editSessionVisible s
        } )

getNewSessionEditor :: Lens' AppState (E.Editor String Name)
getNewSessionEditor = lens newSessionEditor
    (\ s e -> AppState
        { focusRing = focusRing s
        , sessionList = sessionList s
        , colormodeList = colormodeList s
        , newSessionEditor = e
        , escapeToShell = escapeToShell s
        , editSessionVisible = editSessionVisible s
        } )

getColormodeList :: Lens' AppState (L.List Name Colormode)
getColormodeList = lens colormodeList
    (\ s l -> AppState
        { focusRing = focusRing s
        , sessionList = sessionList s
        , colormodeList = l
        , newSessionEditor = newSessionEditor s
        , escapeToShell = escapeToShell s
        , editSessionVisible = editSessionVisible s
        } )

getEscapeFlag :: Lens' AppState Bool
getEscapeFlag = lens escapeToShell
    (\ s f -> AppState
        { focusRing = focusRing s
        , sessionList = sessionList s
        , colormodeList = colormodeList s
        , newSessionEditor = newSessionEditor s
        , escapeToShell = f
        , editSessionVisible = editSessionVisible s
        } )

getEditSessionVisible :: Lens' AppState Bool
getEditSessionVisible = lens editSessionVisible
    (\ s e -> AppState
        { focusRing = focusRing s
        , sessionList = sessionList s
        , colormodeList = colormodeList s
        , newSessionEditor = newSessionEditor s
        , escapeToShell = escapeToShell s
        , editSessionVisible = e
        } )

selAttr :: A.AttrName
selAttr = L.listSelectedAttr <> A.attrName "selected"

nonSelAttr :: A.AttrName
nonSelAttr = L.listAttr

appAttr :: A.AttrMap
appAttr = A.attrMap V.defAttr
    [ (L.listAttr,          V.white `on` V.black)
    , (L.listSelectedAttr,  V.black `on` V.white)
    , (selAttr,             V.withStyle (V.black `on` V.white) V.underline )
    , (E.editFocusedAttr,   V.white `on` V.black)
    ]

appWidth :: Int
appWidth = 35
appHeight :: Int
appHeight = 5

colorHeight :: Int
colorHeight = 2

editorTextHeight :: Int
editorTextHeight = 1

editorDialogHeight :: Int
editorDialogHeight = 10

defaultColormode :: Colormode
defaultColormode = BLUEMODE

itemWidget :: Bool -> String -> Widget Name
itemWidget selected x =
    let selStr s = if selected
            then withAttr selAttr ( str s )
            else withAttr nonSelAttr ( str s )
    in C.hCenter $ str "Session " <+> selStr x

mainUIWidget :: AppState -> Widget Name
mainUIWidget s = ui
    where
        item_list = sessionList s
        color_list = colormodeList s
        box_label = str "Session " <+> cur <+> str " of " <+> total
        color_label = str "Colormode"

        cur = case item_list^.L.listSelectedL of
            Nothing -> str "-"
            Just i  -> str $ show (i+1)

        cur_color = case L.listSelectedElement color_list of
            Nothing -> str "Select Colormode"
            Just (_,c) -> str $ show c

        total = str $ show $ Vec.length $ item_list^.L.listElementsL

        list_box = B.borderWithLabel box_label $
            hLimit appWidth $
            vLimit appHeight $
            F.withFocusRing (s^.getFocusRing) (L.renderList itemWidget) (s^.getSessionList)
        color_box = B.borderWithLabel color_label $
            hLimit appWidth $
            vLimit colorHeight $
            C.hCenter $ str "< " <+> cur_color <+> str " >"
        ui = C.vCenter $ vBox
            [ C.hCenter list_box
            , C.hCenter color_box
            , str " "
            , C.hCenter $ str "Press Enter to attach to selected session."
            , C.hCenter $ str "Press n to create a new session."
            , C.hCenter $ str "Press d to delect selected session."
            , C.hCenter $ str "Press Esc to escape to shell."
            ]

editorTextWidget :: [String] -> Widget Name
editorTextWidget xs = withAttr E.editFocusedAttr $ str $ concat xs

newSessionWidget :: AppState -> Widget Name
newSessionWidget s = ui
    where
        title = str "Enter Session Name"
        ed = B.borderWithLabel title $
            vLimit editorTextHeight $
            F.withFocusRing (s^.getFocusRing) (E.renderEditor editorTextWidget) (s^.getNewSessionEditor)
        
        ui = C.center $
            hLimit appWidth $
            vLimit editorDialogHeight$
            vBox
            [ C.hCenter ed
            , str ""
            , C.hCenter $ str "Press Enter to create session."
            , C.hCenter $ str "Press Esc to cancel."
            ]


drawUI :: AppState -> [Widget Name]
drawUI s = ( [ newSessionWidget s | editSessionVisible s ] )
    <> [ mainUIWidget s ]

mainEventHandler :: T.BrickEvent Name e -> T.EventM Name AppState ()
mainEventHandler (T.VtyEvent e) =
    case e of
        V.EvKey V.KEnter [] -> do
            zoom getSessionList $ do
                sel_session <- use L.listSelectedL
                case sel_session of
                    Nothing -> return ()
                    Just sess -> M.halt
        V.EvKey V.KEsc [] ->
            zoom getEscapeFlag $
                modify (const True) >> M.halt
        V.EvKey (V.KChar ',') [] ->
            zoom getColormodeList $ do
                sel <- use L.listSelectedL
                ls <- use L.listElementsL
                let l = Vec.length ls
                case sel of
                    Nothing -> return ()
                    Just i  -> modify ( L.listMoveTo ( (i+1) `mod` l) )
        V.EvKey (V.KChar '.') [] ->
            zoom getColormodeList $ do
                sel <- use L.listSelectedL
                ls <- use L.listElementsL
                let l = Vec.length ls
                case sel of
                    Nothing -> return ()
                    Just i  -> modify ( L.listMoveTo ( (i-1) `mod` l) )
        V.EvKey (V.KChar 'n') [] -> do
            zoom getEditSessionVisible (T.put True)
            zoom getFocusRing (modify (F.focusSetCurrent NewSessionEditor))
        V.EvKey (V.KChar 'd') [] -> do
            zoom getSessionList $ do
                sel_session <- fmap L.listSelectedElement T.get 
                case sel_session of
                    Nothing -> return ()
                    Just (i,sess) -> do
                        liftIO $ killTmuxSession sess
                        new_session_list <- liftIO $ fmap Vec.fromList tmuxGetSessions
                        modify (L.listReplace new_session_list (Just i))
        ev -> zoom getSessionList (L.handleListEvent ev)
mainEventHandler _ = return ()

sessionDialogEventHandler :: T.BrickEvent Name e -> T.EventM Name AppState ()
sessionDialogEventHandler (T.VtyEvent (V.EvKey V.KEnter [])) = do
    s <- T.get
    let session_name = currentLine $ s ^. (getNewSessionEditor . E.editContentsL)
    zoom getNewSessionEditor $ do
        unless (session_name == "") $ liftIO $ makeTmuxSession session_name
        modify (E.applyEdit clearZipper)
    zoom getSessionList $ do
        new_session_list <- liftIO tmuxGetSessions
        let target_list =
                        let l = L.list SessionList (Vec.fromList new_session_list) 1
                        in L.listMoveToElement session_name l
        T.put target_list
    zoom getEditSessionVisible (T.put False)
    zoom getFocusRing (modify (F.focusSetCurrent SessionList))

sessionDialogEventHandler (T.VtyEvent (V.EvKey V.KEsc [])) = do
    zoom getEditSessionVisible (T.put False)
    zoom getFocusRing (modify (F.focusSetCurrent SessionList))

sessionDialogEventHandler ev = zoom getNewSessionEditor (E.handleEditorEvent ev)

appEvent :: T.BrickEvent Name e -> T.EventM Name AppState ()
appEvent e = do
    s <- T.get
    let f = s ^. getFocusRing
    case F.focusGetCurrent f of
        Nothing -> return ()
        Just SessionList -> mainEventHandler e
        Just ColormodeList -> 
            liftIO $ ioError $ userError "Colormode list should never be focused!"
        Just NewSessionEditor -> sessionDialogEventHandler e

getInitialColormode :: IO (Maybe Colormode)
getInitialColormode = do
    res <- tmuxSafeQueryEnvVariable "DARKMODE"
    case res of
        Just "-1"   -> return $ Just LIGHTMODE
        Just "0"    -> return $ Just BLUEMODE
        Just "1"    -> return $ Just DARKMODE
        _           -> return Nothing

getInitialState :: IO AppState
getInitialState = do
    sessions <- tmuxGetSessions
    initialColormode <- fmap (fromMaybe defaultColormode) getInitialColormode
    let colormodes = [ DARKMODE, LIGHTMODE, BLUEMODE ]
    let colormodeList = L.list ColormodeList (Vec.fromList colormodes) 1
    return $ AppState
        { focusRing = F.focusRing [ SessionList, ColormodeList, NewSessionEditor ]
        , sessionList = L.list SessionList (Vec.fromList sessions) 1
        , colormodeList = L.listMoveToElement initialColormode colormodeList 
        , newSessionEditor = E.editor NewSessionEditor Nothing ""
        , escapeToShell = False
        , editSessionVisible = False
        }

app :: M.App AppState e Name
app =
    M.App   { M.appDraw = drawUI
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent = appEvent
            , M.appStartEvent = return ()
            , M.appAttrMap = const appAttr
            }

brick :: IO ()
brick = do
    initialState <- getInitialState
    finalState <- M.defaultMain app initialState
    let escape = escapeToShell finalState
    if escape
        then ioError $ userError "Escaped to shell."
        else do
            let
                maybe_session = L.listSelectedElement (sessionList finalState)
                maybe_colormode = L.listSelectedElement (colormodeList finalState)
            case maybe_session of
                Nothing -> ioError $ userError "No session was selected!"
                Just (_, s) -> enterTmuxSession s (maybe defaultColormode snd maybe_colormode )
