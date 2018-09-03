module Torus1.Triangles
  (allTriangles, NTriangle)
  where
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Utils.Normals                (triangleNormal)

type NTriangle = ((Vertex3 Double, Vertex3 Double, Vertex3 Double), Normal3 Double)

fun :: Double -> Double -> Double -> Double -> Vertex3 Double
fun a nlobes u v = Vertex3 x y z
  where
    a1 = pi/2 - (pi/2-a) * cos u
    sina1 = sin a1
    a2 = u/nlobes + a*sin(2*u)
    p2 = sina1 * cos a2
    p3 = sina1 * sin a2
    p1 = cos a1
    yden = sqrt(2*(1+p1))
    y1 = (1+p1)/yden
    y2 = p2/yden
    y3 = p3/yden
    cosphi = cos v
    sinphi = sin v
    x4 = cosphi * y1
    x3 = sinphi * y1
    x2 = cosphi*y2 - sinphi*y3
    x1 = cosphi*y3 + sinphi*y2
    x = x1/(1-x4)
    y = x2/(1-x4)
    z = x3/(1-x4)

triangle :: (Double -> Double -> Vertex3 Double)
         -> [Double] -> [Double] -> Int -> Int
         -> [NTriangle]
triangle f u_ v_ i j = [((a, b, c), normal1), ((c, b, d), normal2)]
  where
  (a,c,d,b) = ( f (u_!!i) (v_!!j)
              , f (u_!!i) (v_!!(j+1))
              , f (u_!!(i+1)) (v_!!(j+1))
              , f (u_!!(i+1)) (v_!!j) )
  normal1 = triangleNormal (a, b, c)
  normal2 = triangleNormal (c, b, d)

allTriangles :: Int -> Double -> Double -> [NTriangle]
allTriangles m a nlobes =
    concatMap (uncurry (triangle (fun a nlobes) sequ seqv))
        [(i,j) | i <- [0 .. m-1], j <- [0 .. m-1]]
        where
            sequ,seqv :: [Double]
            sequ = [2*nlobes*pi * frac i m | i <- [0 .. m]]
            seqv = [2*pi * frac i m | i <- [0 .. m]]
            frac :: Int -> Int -> Double
            frac p q = realToFrac p / realToFrac q
