{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick
import qualified Brick.AttrMap as A
import Brick.BChan
import Brick.Focus (focusGetCurrent)
import qualified Brick.Focus as F
import Brick.Main (App (..), ViewportScroll, halt)
import Brick.Types (ViewportType (Both))
import qualified Brick.Types as T
import Brick.Widgets.Border (border, borderWithLabel, vBorder)
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Border.Style (unicode, unicodeRounded)
import Brick.Widgets.Center (center, centerLayer)
import Brick.Widgets.Edit (getEditContents)
import qualified Brick.Widgets.Edit as Edit
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as C
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Text.Zipper.Generic.Words as TZ
import Data.Traversable (for)
import GHC.IO (evaluate, throwIO)
import Graphics.Vty
import qualified Graphics.Vty.Attributes as V
import Graphics.Vty.Config
import qualified Graphics.Vty.Input.Events as V
import Graphics.Vty.Platform.Unix (mkVtyWithSettings)
import Graphics.Vty.Platform.Unix.Settings (UnixSettings (UnixSettings, settingInputFd), VtyUnixConfigurationError (MissingTermEnvVar), defaultSettings)
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..))
import System.Hclip (setClipboard)
import System.IO
import System.Posix.IO.ByteString
import System.Posix.Terminal
import System.Posix.Types
import System.Process (CreateProcess (..), StdStream (CreatePipe), createProcess, proc, waitForProcess)

data Name
  = QueryEditor
  | LeftView
  | RightView
  deriving (Eq, Ord, Show)

data Executable = Executable {_exeFilePath :: FilePath, _exeArgs :: [String]} deriving (Show, Eq, Ord)

makeLenses ''Executable

data ProcessResult = ProcessSuccess !Text | ProcessFailure !Int !Text deriving (Eq, Ord, Show)

data St = St
  { _inputEditor :: !(Edit.Editor Text Name),
    _leftViewTxt :: !Text,
    _rightViewTxt :: !Text,
    _focusRing :: !(F.FocusRing Name),
    _lastError :: !(Maybe Text),
    _initialInput :: !Text,
    _statusMessage :: !(Maybe Text),
    _executable :: !Executable
  }

makeLenses ''St

mkSt :: Text -> Executable -> St
mkSt input = St (Edit.editorText QueryEditor (Just 1) "") input "" (F.focusRing [QueryEditor, LeftView, RightView]) Nothing input Nothing

drawUI :: St -> [Widget Name]
drawUI st = errorDialog ++ pure mainWidget
  where
    focused = focusGetCurrent (st ^. focusRing)
    errorDialog = maybeToList $ fmap (\errOutput -> centerLayer (border $ txt errOutput)) (st ^. lastError)
    mainWidget = withBorderStyle unicode $ queryPane <=> dualPane <=> statusBar <=> keysBar
    queryPane =
      editIsFocused $ border $ txt "> " <+> F.withFocusRing (st ^. focusRing) (Edit.renderEditor (txt . Text.unlines)) (st ^. inputEditor)

    statusBar = txt $ fromMaybe " " (st ^. statusMessage)
    keysBar = txt "RET: execute | C-y: copy query | C-s: copy output | C-c: exit | M-Ret: commit to left side | M-Backspace: revert left side"
    dualPane =
      leftIsFocused (border (withVScrollBars OnLeft $ viewport LeftView Both (txt (st ^. leftViewTxt))))
        <+> rightIsFocused (border (withVScrollBars OnRight $ viewport RightView Both (txt (st ^. rightViewTxt))))
    leftIsFocused = if focused == Just LeftView then overrideAttr B.borderAttr borderFocusAttr else id
    rightIsFocused = if focused == Just RightView then overrideAttr B.borderAttr borderFocusAttr else id
    editIsFocused = if focused == Just QueryEditor then overrideAttr B.borderAttr borderFocusAttr else id

vpEvent :: ViewportScroll Name -> T.BrickEvent Name e -> T.EventM Name x ()
vpEvent vpScroll (T.VtyEvent (V.EvKey V.KDown [])) = vScrollBy vpScroll 1
vpEvent vpScroll (T.VtyEvent (V.EvKey V.KUp [])) = vScrollBy vpScroll (-1)
vpEvent vpScroll (T.VtyEvent (V.EvKey V.KRight [])) = hScrollBy vpScroll 1
vpEvent vpScroll (T.VtyEvent (V.EvKey V.KLeft [])) = hScrollBy vpScroll (-1)
vpEvent vpScroll (T.VtyEvent (V.EvKey (V.KChar 'j') [])) = vScrollBy vpScroll 1
vpEvent vpScroll (T.VtyEvent (V.EvKey (V.KChar 'k') [])) = vScrollBy vpScroll (-1)
vpEvent vpScroll (T.VtyEvent (V.EvKey (V.KChar 'l') [])) = hScrollBy vpScroll 1
vpEvent vpScroll (T.VtyEvent (V.EvKey (V.KChar 'h') [])) = hScrollBy vpScroll (-1)
vpEvent vpScroll (T.VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) = vScrollBy vpScroll 10 -- TODO: use height / 2
vpEvent vpScroll (T.VtyEvent (V.EvKey (V.KChar 'u') [V.MCtrl])) = vScrollBy vpScroll (-10) -- TODO: use height / 2
vpEvent _ _ = return ()

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent ev = do
  clearStatusMessage
  err <- use lastError
  case err of
    Just _ -> lastError .= Nothing
    Nothing -> case ev of
      (T.VtyEvent (V.EvKey V.KBS [V.MMeta])) -> uncommitOutput
      (T.VtyEvent (V.EvKey V.KEnter [V.MMeta])) -> commitOutput
      (T.VtyEvent (V.EvKey V.KEnter [])) -> use executable >>= executeProcess
      (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) -> halt
      (T.VtyEvent (V.EvKey (V.KChar '\t') [])) -> focusRing %= F.focusNext
      (T.VtyEvent (V.EvKey V.KBackTab [])) -> focusRing %= F.focusPrev
      (T.VtyEvent (V.EvKey (V.KChar 'y') [V.MCtrl])) -> copyQuery
      (T.VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) -> copyOutput
      _ -> dispatchEvent ev

clearStatusMessage :: EventM Name St ()
clearStatusMessage = statusMessage .= Nothing

copyQuery :: EventM Name St ()
copyQuery = do
  contents <- use inputEditor <&> Text.intercalate "\n" . getEditContents
  liftIO $ setClipboard (Text.unpack contents)
  statusMessage ?= "Query copied"

copyOutput :: EventM Name St ()
copyOutput = do
  contents <- use rightViewTxt
  liftIO $ setClipboard (Text.unpack contents)
  statusMessage ?= "Output copied"

commitOutput :: EventM Name St ()
commitOutput = do
  r <- use rightViewTxt
  leftViewTxt .= r
  invalidateCacheEntry LeftView

uncommitOutput :: EventM Name St ()
uncommitOutput = do
  initial <- use initialInput
  leftViewTxt .= initial
  invalidateCacheEntry LeftView

executeProcess :: Executable -> EventM Name St ()
executeProcess exe = do
  argument <- use inputEditor <&> Text.intercalate "\n" . getEditContents
  input <- use initialInput
  processResult <- liftIO $ do
    let processSpec = (proc (exe ^. exeFilePath) (exe ^. exeArgs ++ [Text.unpack argument])) {std_out = CreatePipe, std_in = CreatePipe, std_err = CreatePipe}
    (Just procStdin, Just procStdout, Just procStderr, ph) <- createProcess processSpec
    TIO.hPutStr procStdin input
    hClose procStdin
    exitCode <- waitForProcess ph
    case exitCode of
      ExitSuccess -> ProcessSuccess <$> TIO.hGetContents procStdout
      ExitFailure code -> ProcessFailure code <$> TIO.hGetContents procStderr
  case processResult of
    ProcessSuccess output -> do
      statusMessage ?= "Success"
      rightViewTxt .= output >> invalidateCacheEntry RightView
    ProcessFailure code output -> do
      statusMessage ?= ("Failed with code: " <> Text.pack (show code))
      lastError ?= output

dispatchEvent :: BrickEvent Name e -> EventM Name St ()
dispatchEvent ev = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just QueryEditor ->
      case ev of
        (T.VtyEvent (V.EvKey (V.KChar 'w') [V.MCtrl])) -> inputEditor %= Edit.applyEdit (TZ.deleteWord . TZ.moveWordLeft)
        _ -> zoom inputEditor $ Edit.handleEditorEvent ev
    Just LeftView -> vpEvent (viewportScroll LeftView) ev
    Just RightView -> vpEvent (viewportScroll RightView) ev
    Nothing -> return ()

appCursor :: St -> [CursorLocation Name] -> Maybe (CursorLocation Name)
appCursor = F.focusRingCursor (^. focusRing)

borderFocusAttr = attrName "borderFocused"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (borderFocusAttr, V.defAttr `withForeColor` V.yellow `withStyle` V.bold)
    ]

theApp :: App St e Name
theApp =
  App
    { appDraw = drawUI,
      appChooseCursor = appCursor,
      appHandleEvent = appEvent,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

main :: IO ()
main = do
  args <- getArgs
  let exe = case args of
        executable : exeArgs -> Executable executable exeArgs
        [] -> Executable "jq" []
  input <- TIO.getContents
  void $ evaluate input
  _ <- hClose stdin
  terminalName <- getControllingTerminalName
  terminalFd <- openFd (C.pack terminalName) ReadOnly defaultFileFlags
  channel <- newBChan 42
  initialVty <- buildVty terminalFd
  _ <- customMain initialVty (buildVty terminalFd) (Just channel) theApp (mkSt input exe)
  return ()
  where
    buildVty :: Fd -> IO Vty
    buildVty terminalFd' = do
      termName <- lookupEnv "TERM"
      case termName of
        Nothing -> throwIO MissingTermEnvVar
        Just t -> do
          let settings = UnixSettings 1 100 terminalFd' stdOutput t
          v <- mkVtyWithSettings defaultConfig $ settings {settingInputFd = terminalFd'}
          when (supportsMode (outputIface v) Mouse) $ setMode (outputIface v) Mouse True
          when (supportsMode (outputIface v) BracketedPaste) $ setMode (outputIface v) BracketedPaste True
          return v
