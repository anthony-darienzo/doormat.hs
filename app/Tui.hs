module Tui ( brick ) where

import qualified Pipes as Pipe

import System.IO.Error (mkIOError, doesNotExistErrorType)
import Shell
    ( tmuxSafeQueryEnvVariable
    , enterTmuxSession
    , tmuxGetSessions
    , Colormode(..)
    , makeTmuxSession
    , killTmuxSession
    )
import Titlecard (titleString)

import Lens.Micro ((^.), Lens', lens)
import Lens.Micro.Mtl ( use, zoom )
import Control.Monad.State (void, forever, modify, liftIO, unless)
import Control.Concurrent (forkIO, threadDelay)
import Data.Maybe (fromMaybe, isJust)
import qualified Graphics.Vty as V

import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.BChan as BC
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
  , raw )
import Brick.Util (fg, on)
import qualified Brick.Widgets.Edit as E
import Data.Vector.Generic.New (New(New))
import Data.Text.Zipper (currentLine, clearZipper)

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

refColormodeList :: L.GenericList Name Vec.Vector Colormode
refColormodeList = L.list ColormodeList (Vec.fromList colormodes) 1
    where colormodes = [ DARKMODE, LIGHTMODE, BLUEMODE ]

appAttr :: A.AttrMap
appAttr = A.attrMap V.defAttr
    [ (L.listAttr,          V.white `on` V.black)
    , (L.listSelectedAttr,  V.black `on` V.white)
    , (selAttr,             V.withStyle (V.black `on` V.white) V.underline )
    , (E.editFocusedAttr,   V.white `on` V.black)
    ]

data Name = SessionList | ColormodeList | NewSessionEditor deriving (Ord, Eq, Show)

data PipeTick = Tick

data AppState = AppState
    { focusRing :: F.FocusRing Name
    , sessionList :: L.List Name String
    , colormodeList :: L.List Name Colormode
    , newSessionEditor :: E.Editor String Name
    , escapeToShell :: Bool
    , editSessionVisible :: Bool
    , pipeImg :: V.Image
    , pipeImgStream :: IO V.Image
    }

getFocusRing :: Lens' AppState (F.FocusRing Name)
getFocusRing = lens focusRing
    (\ s r -> s { focusRing = r })

getSessionList :: Lens' AppState (L.List Name String)
getSessionList = lens sessionList
    (\ s l -> s { sessionList = l })

getNewSessionEditor :: Lens' AppState (E.Editor String Name)
getNewSessionEditor = lens newSessionEditor
    (\ s e -> s { newSessionEditor = e })

getColormodeList :: Lens' AppState (L.List Name Colormode)
getColormodeList = lens colormodeList
    (\ s l -> s { colormodeList = l })

getEscapeFlag :: Lens' AppState Bool
getEscapeFlag = lens escapeToShell
    (\ s f -> s { escapeToShell = f })

getEditSessionVisible :: Lens' AppState Bool
getEditSessionVisible = lens editSessionVisible
    (\ s e -> s { editSessionVisible = e })

getPipeImg :: Lens' AppState V.Image
getPipeImg = lens pipeImg
    (\ s i -> s { pipeImg = i })

getPipeImgStream :: Lens' AppState (IO V.Image)
getPipeImgStream = lens pipeImgStream
    (\ s is -> s { pipeImgStream = is })

selAttr :: A.AttrName
selAttr = L.listSelectedAttr <> A.attrName "selected"

nonSelAttr :: A.AttrName
nonSelAttr = L.listAttr

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
        ui = C.vCenterLayer $ vBox
            [ C.hCenterLayer $ str titleString
            , str " "
            , C.hCenterLayer list_box
            , C.hCenterLayer color_box
            , str " "
            , C.hCenterLayer $ str "Press Enter to attach to selected session."
            , C.hCenterLayer $ str "Press n to create a new session."
            , C.hCenterLayer $ str "Press d to delete selected session."
            , C.hCenterLayer $ str "Press Esc to escape to shell."
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
    <> [ raw $ pipeImg s ]
    

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

getInitialColormode :: IO (Maybe Colormode)
getInitialColormode = do
    res <- tmuxSafeQueryEnvVariable "INITIALDARKMODE"
    if isJust res
        then return $ numberToMaybeLabel res
        else do
            res' <- tmuxSafeQueryEnvVariable "DARKMODE"
            return $ numberToMaybeLabel res'
    where
        numberToMaybeLabel (Just "-1") = Just LIGHTMODE
        numberToMaybeLabel (Just "0")  = Just BLUEMODE
        numberToMaybeLabel (Just "1")  = Just DARKMODE
        numberToMaybeLabel _           = Nothing

getInitialState :: IO AppState
getInitialState = do
    sessions <- tmuxGetSessions
    initialColormode <- fmap (fromMaybe defaultColormode) getInitialColormode
    return $ AppState
        { focusRing = F.focusRing [ SessionList, ColormodeList, NewSessionEditor ]
        , sessionList = L.list SessionList (Vec.fromList sessions) 1
        , colormodeList = L.listMoveToElement initialColormode refColormodeList
        , newSessionEditor = E.editor NewSessionEditor Nothing ""
        , escapeToShell = False
        , editSessionVisible = False
        , pipeImg = V.emptyImage 
        , pipeImgStream = pure V.emptyImage }

createPipeImgStream :: T.EventM Name AppState (IO V.Image)
createPipeImgStream = do
    vty <- M.getVtyHandle
    let iface = V.outputIface vty
    dbounds <- liftIO $ V.displayBounds iface
    imgStream <- liftIO $ Pipe.randomPipeStream dbounds
    return imgStream

appEvent :: T.BrickEvent Name PipeTick -> T.EventM Name AppState ()
appEvent e = do
    s <- T.get
    case e of
        T.AppEvent Tick -> do
            let is = s ^. getPipeImgStream
            zoom getPipeImg $ do
                new_img <- liftIO is
                T.put new_img
        _ -> do
            let f = s ^. getFocusRing
            case F.focusGetCurrent f of
                Nothing -> return ()
                Just SessionList -> mainEventHandler e
                Just ColormodeList ->
                    liftIO $ ioError $ userError "Colormode list should never be focused!"
                Just NewSessionEditor -> sessionDialogEventHandler e

startEvent :: T.EventM Name AppState ()
startEvent = do
    imgStream <- createPipeImgStream    
    zoom getPipeImgStream $ do
        T.put imgStream

app :: M.App AppState PipeTick Name
app =
    M.App   { M.appDraw = drawUI
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent = appEvent
            , M.appStartEvent = startEvent
            , M.appAttrMap = const appAttr
            }

brick :: IO ()
brick = do
    initialState <- getInitialState
    chan <- BC.newBChan 10
    void . forkIO $ forever $ do
        BC.writeBChan chan Tick
        threadDelay Pipe.tickrate
    let vtyBuilder = V.mkVty V.defaultConfig
    vty <- vtyBuilder
    finalState <- M.customMain vty vtyBuilder (Just chan) app initialState
    if escapeToShell finalState
        then ioError $ userError "Escaped to shell."
        else do
            let
                maybe_session = L.listSelectedElement (sessionList finalState)
                target_colormode = 
                    let maybe_colormode = L.listSelectedElement (colormodeList finalState)
                    in maybe defaultColormode snd maybe_colormode
            case maybe_session of
                Nothing -> ioError $ userError "No session was selected!"
                Just (_, s) -> enterTmuxSession s target_colormode
