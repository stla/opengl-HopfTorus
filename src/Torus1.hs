module Torus1
  (main)
  where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           System.Directory                  (doesDirectoryExist)
import           Text.Printf
import           Torus1.Triangles


htorus :: Double -> Double -> [NTriangle]
htorus = allTriangles 800

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextTriangles :: IORef [NTriangle]
    }

white,black,pink :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
pink       = Color4    1    0  0.5    1

display :: Context -> IORef GLdouble -> IORef GLfloat -> DisplayCallback
display context zoom alpha = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  triangles <- get (contextTriangles context)
  z <- get zoom
  alpha' <- get alpha
  loadIdentity
  (_, size) <- get viewport
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  rotate alpha' $ Vector3 1 1 1
  renderPrimitive Triangles $
    mapM_ drawTriangle triangles
  swapBuffers
  where
    drawTriangle ((v1,v2,v3),norm) = do
      materialDiffuse Front $= pink
      normal norm
      vertex v1
      vertex v2
      vertex v3

resize :: GLdouble -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (24+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -- zoom
         -> IORef Double -- nlobes
         -> IORef Double -- A
         -> IORef Bool -- animation
         -> IORef [NTriangle]
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom nlobes a anim triangles c _ = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+2)
    'm' -> zoom $~! (+0.1)
    'l' -> zoom $~! subtract 0.1
    'h' -> do
      nlobes $~! (+1)
      nlobes' <- get nlobes
      a' <- get a
      writeIORef triangles (htorus a' nlobes')
    'n' -> do
      nlobes $~! (\n -> if n>1 then n-1 else n)
      nlobes' <- get nlobes
      a' <- get a
      writeIORef triangles (htorus a' nlobes')
    'g' -> do
      nlobes' <- get nlobes
      a $~! (+0.02)
      a' <- get a
      writeIORef triangles (htorus a' nlobes')
    'b' -> do
      nlobes' <- get nlobes
      a $~! subtract 0.02
      a' <- get a
      writeIORef triangles (htorus a' nlobes')
    'a' -> anim $~! not
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

idle :: IORef Bool -> IORef Int -> IORef GLfloat -> IdleCallback
idle anim snapshot alpha = do
    a <- get anim
    when a $ do
      s <- get snapshot
      ppmExists <- doesDirectoryExist "./ppm"
      when (ppmExists && s < 360) $ do
        let ppm = printf "ppm/torus1-%04d.ppm" s
        (>>=) capturePPM (B.writeFile ppm)
      snapshot $~! (+1)
      alpha $~! (+1)
      postRedisplay Nothing
    return ()

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Hopf torus"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= black
  materialAmbient Front $= black
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-500) 1
  ambient (Light 0) $= white
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  alpha <- newIORef 0.0
  let a = 0.44
      nlobes = 3.0
      triangles = htorus a nlobes
  nlobes' <- newIORef nlobes
  a' <- newIORef a
  triangles' <- newIORef triangles
  anim <- newIORef False
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextTriangles = triangles'}
                             zoom alpha
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom nlobes' a' anim triangles')
  snapshot <- newIORef 0
  idleCallback $= Just (idle anim snapshot alpha)
  putStrLn "*** Hopf torus ***\n\
        \    To quit, press q.\n\
        \    Scene rotation: e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease number of lobes: h, n\n\
        \    Increase/decrease A: g, b\n\
        \    Animation: a\n\
        \"
  mainLoop
