This program is adapted from GLFW/example/example.hs to use monad-control library, and is identital to the code from the tutorial [Invert the Inversion of Control](http://www.thev.net/PaulLiu/invert-inversion.html), which has more details. 

To demonstrate the usage of GLFW for OpenGL based Haskell applications, here is a sample program that allows user to draw lines by holding the left mouse button and move the mouse.

\begin{code}
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW.Task
import Graphics.Rendering.OpenGL (($=))
import Control.Monad.Task
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad
import Prelude hiding (lines)

main = do
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.5
  -- set the color to clear background
  GL.clearColor $= GL.Color4 0 0 0 0
  -- run the main app
  lineTask
  -- finish up
  GLFW.closeWindow
  GLFW.terminate

set2DViewport size@(GL.Size w h) = do
  GL.viewport   $= (GL.Position 0 0, size)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

type Lines = [(GL.GLint, GL.GLint)]

drawLines :: Lines -> IO ()
drawLines lines = do
  GL.clear [GL.ColorBuffer]
  GL.color $ GL.Color3 1 0 (0::GL.GLdouble)
  GL.renderPrimitive GL.Lines $ mapM_ 
    (\ (x, y) -> GL.vertex (GL.Vertex3 (fromIntegral x) 
                                       (fromIntegral y) 
                                       (0::GL.GLdouble))) lines

data S = S { lines :: Lines, dirty :: Bool }
type M a = TaskT Event (StateT S IO) a

getLines      = fmap lines get
modifyLines f = modify $ \x -> x { lines = f (lines x), dirty = True }
getDirty      = fmap dirty get
putDirty y    = modify $ \x -> x { dirty = y }

repeatUntil f m = loop
  where loop = m >> watch Just >>= maybe loop return . f

buttonPress   = onButton >=> isPress
buttonRelease = onButton >=> isRelease
isESC = onKey >=> isPress >=> isKey GLFW.ESC 

lineTask :: IO ()
lineTask = (`evalStateT` (S [] False)) . runTask $ do
  -- here the monad is of type M ()
  waitForEvents <- liftIO registerTaskCallbacks
  fork $ forever $ watch onSize >>= liftIO . set2DViewport
  fork $ forever $ watch onClose >> exit
  fork $ forever $ watch onRefresh >> putDirty True
  fork $ forever $ watch isESC >> exit
  fork $ forever $ interaction
  forever $ do
    waitForEvents
    d <- getDirty
    when d $ getLines >>= liftIO . drawLines >> liftIO GLFW.swapBuffers
    putDirty False
    yield
  where
    interaction = do
      watch buttonPress
      (GL.Position x y) <- liftIO $ GL.get GLFW.mousePos
      modifyLines (((x,y):) . ((x,y):)) 
      repeatUntil buttonRelease $ do
        (GL.Position x y) <- liftIO $ GL.get GLFW.mousePos
        modifyLines (((x,y):) . tail) 
\end{code}

