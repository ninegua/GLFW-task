{- | GLFW helper functions for use with a 'Control.Monad.Task.TaskT' monad transformer (from monad-task package).
-}
{-# LANGUAGE FlexibleContexts, TupleSections #-}
module Graphics.UI.GLFW.Task 
  (
    Event(..)
  , onKey
  , onChar
  , onButton
  , onPos
  , onWheel
  , onSize
  , onClose
  , onRefresh
  , isKey
  , isChar
  , isButton
  , isPress
  , isRelease
  , registerTaskCallbacks
  ) where

import Control.Concurrent
import Graphics.UI.GLFW 
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad.IO.Class 
import Control.Monad.Task.Class 

-- | @Event@ is a unified data type for all GLFW events.
data Event 
  = KeyEvent Key KeyButtonState
  | CharEvent Char KeyButtonState
  | MouseButtonEvent MouseButton KeyButtonState
  | MousePosEvent GL.Position 
  | MouseWheelEvent Int
  | WindowSizeEvent GL.Size 
  | WindowCloseEvent 
  | WindowRefreshEvent
  deriving (Eq, Show)


onKey     :: Event -> Maybe (Key, KeyButtonState)
onKey     (KeyEvent k s)         = Just (k, s)
onKey     _                      = Nothing
onChar    :: Event -> Maybe (Char, KeyButtonState)
onChar    (CharEvent ch s)       = Just (ch, s)
onChar    _                      = Nothing
onButton  :: Event -> Maybe (MouseButton, KeyButtonState)
onButton  (MouseButtonEvent b s) = Just (b, s)
onButton  _                      = Nothing
onPos     :: Event -> Maybe GL.Position
onPos     (MousePosEvent p)      = Just p
onPos     _                      = Nothing
onWheel   :: Event -> Maybe Int
onWheel   (MouseWheelEvent w)    = Just w
onWheel   _                      = Nothing
onSize    :: Event -> Maybe GL.Size
onSize    (WindowSizeEvent s)    = Just s
onSize    _                      = Nothing
onClose   :: Event -> Maybe ()
onClose   WindowCloseEvent       = Just ()
onClose   _                      = Nothing
onRefresh :: Event -> Maybe ()
onRefresh WindowRefreshEvent     = Just ()
onRefresh _                      = Nothing

isKey :: Enum a => a -> Key -> Maybe ()
isKey key k | fromEnum k == fromEnum key = Just ()
            | otherwise                  = Nothing

isChar :: Char -> Char -> Maybe ()
isChar ch c | ch == c   = Just ()
            | otherwise = Nothing

isButton :: MouseButton -> MouseButton -> Maybe ()
isButton but b | b == but  = Just ()
               | otherwise = Nothing

isPress, isRelease :: (a, KeyButtonState) -> Maybe a
isPress   (b, s) | s == Press   = Just b 
                 | otherwise    = Nothing

isRelease (b, s) | s == Release = Just b 
                 | otherwise    = Nothing

-- | @registerTaskCallbacks@ sets up all event callbacks, and returns a 
--   'Graphics.UI.GLFW.waitEvent' equivalent function for task monad,
--   which must be called repeatedly in order to pump events to other 
--   task co-routines.
--
--   These task co-routines should use 'Control.Monad.Task.Class.watch' 
--   to select event of interest, and they should be forked prior to
--   the waitEvent call.
registerTaskCallbacks :: (MonadIO m, MonadTask Event m) => IO (m ())
registerTaskCallbacks = do
  q <- newMVar []
  let enqueue = modifyMVar_ q . (return .) . (:)
      dequeue = modifyMVar q $ return . ([],)
  windowRefreshCallback $= enqueue WindowRefreshEvent
  windowSizeCallback    $= enqueue . WindowSizeEvent
  windowCloseCallback   $= (enqueue WindowCloseEvent >> return True)
  keyCallback           $= (enqueue .) . KeyEvent
  charCallback          $= (enqueue .) . CharEvent
  mouseButtonCallback   $= (enqueue .) . MouseButtonEvent
  mousePosCallback      $= enqueue . MousePosEvent
  mouseWheelCallback    $= enqueue . MouseWheelEvent
  return (liftIO (waitEvents >> dequeue) >>= mapM_ signal . reverse)
