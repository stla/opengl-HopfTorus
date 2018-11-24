module Cone.TestTransfoCone
  where
import           Data.Tuple.Extra             (fst3, snd3, thd3, second)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Linear                       (V3 (..))
import           Cone.TransfoCone
import           Control.Monad                (forM_)

cr1 :: V3 GLfloat
cr1 = V3 0 0 0

phi,a,b,c :: GLfloat
phi = (1 + sqrt 5) / 2
a = 1 / sqrt 3
b = a / phi
c = a * phi

points :: [V3 GLfloat]
points = 
   [V3 a a a,
    V3 a a (-a),
    V3 a (-a) a,
    V3 (-a) (-a) a,
    V3 (-a) a (-a),
    V3 (-a) a a,
    V3 0 b (-c),
    V3 0 (-b) (-c),
    V3 0 (-b) c,
    V3 c 0 (-b),
    V3 (-c) 0 (-b),
    V3 (-c) 0 b,
    V3 b c 0,
    V3 b (-c) 0,
    V3 (-b) (-c) 0,
    V3 (-b) c 0,
    V3 0 b c,
    V3 a (-a) (-a),
    V3 c 0 b,
    V3 (-a) (-a) (-a)]

r2 :: GLfloat
r2 = 0.2

white,black,blue :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
blue       = Color4    0    0    1    1

tmatsAndHeights :: [([GLfloat], GLdouble)]
tmatsAndHeights = 
    map (second realToFrac . transfoMatrixCone (realToFrac r2) cr1)
                   points

display :: DisplayCallback
display = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  forM_ tmatsAndHeights $ \tmatAndHeight ->
    preservingMatrix $ do
      m <- newMatrix RowMajor (fst tmatAndHeight) :: IO (GLmatrix GLfloat)
      multMatrix m
      materialDiffuse Front $= blue
      renderObject Solid $ Cone (realToFrac r2) (snd tmatAndHeight) 16 16
  swapBuffers

resize :: Size -> IO ()
resize s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 2 2 (-3)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Cones"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 200 200 (-300) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Less
  shadeModel $= Smooth
  displayCallback $= display
  reshapeCallback $= Just resize
  idleCallback $= Nothing
  mainLoop