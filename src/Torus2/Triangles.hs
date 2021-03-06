module Torus2.Triangles
  (allTriangles, NTriangle)
  where
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Data.Array                   (Array, (!), array)
import qualified Data.Array as A

type Point = (Double, Double, Double)
type Vector = (Double, Double, Double)
type NPoint = (Vertex3 Double, Normal3 Double)
type NTriangle = (NPoint, NPoint, NPoint)

pointToVertex3 :: Point -> Vertex3 Double
pointToVertex3 (x,y,z) = Vertex3 x y z

vectorToNormal3 :: Vector -> Normal3 Double
vectorToNormal3 (x,y,z) = Normal3 x y z

fun :: Double -> Double -> Double -> Double -> Point
fun a nlobes u v = (x1/(den-x4), x2/(den-x4), x3/(den-x4))
  where
    a1 = pi/2 - (pi/2-a) * cos(u*nlobes)
    a2 = u + a*sin(2*u*nlobes)
    sina1 = sin a1
    p1 = cos a1
    p2 = sina1 * cos a2
    p3 = sina1 * sin a2
    cosphi = cos v
    sinphi = sin v
    x3 = cosphi * (1+p1)
    x4 = sinphi * (1+p1)
    x2 = cosphi*p2 - sinphi*p3
    x1 = cosphi*p3 + sinphi*p2
    den = sqrt(2*(1+p1))

frac :: Int -> Int -> Double
frac p q = realToFrac p / realToFrac q

allVertices :: (Double -> Double -> Point) -> (Int,Int) -> Array (Int,Int) Point
allVertices f (nu, nv) = array ((0,0), (nu-1,nv-1)) associations
  where
  u_ = [2*pi * frac i nu | i <- [0 .. nu-1]]
  v_ = [2*pi * frac i nv | i <- [0 .. nv-1]]
  indices = [(i,j) | i <- [0 .. nu-1], j <- [0 .. nv-1]]
  g (i,j) = ((i,j), f (u_ !! i) (v_ !! j))
  associations = map g indices

triangleNormal0 :: (Point, Point, Point) -> Vector
triangleNormal0 ((x1,x2,x3), (y1,y2,y3), (z1,z2,z3)) = (a/norm,b/norm,c/norm)
  where
    (a, b, c) = crossProd (z1-x1, z2-x2, z3-x3) (y1-x1, y2-x2, y3-x3) 
    crossProd (a1,a2,a3) (b1,b2,b3) = (a2*b3-a3*b2, a3*b1-a1*b3, a1*b2-a2*b1)
    norm = sqrt(a*a + b*b + c*c)

averageNormals :: Vector -> Vector -> Vector -> Vector -> Vector -> Vector -> Vector
averageNormals (x1,y1,z1) (x2,y2,z2) (x3,y3,z3) (x4,y4,z4) (x5,y5,z5) (x6,y6,z6) = 
  ((x1+x2+x3+x4+x5+x6)/6, (y1+y2+y3+y4+y5+y6)/6, (z1+z2+z3+z4+z5+z6)/6)

normalij :: Array (Int,Int) Point -> (Int, Int) -> Vector
normalij vertices (i,j) = averageNormals n1 n2 n3 n4 n5 n6
  where
  ((_,_), (nu',nv')) = A.bounds vertices
  im1 = if i==0 then nu' else i-1
  ip1 = if i==nu' then 0 else i+1
  jm1 = if j==0 then nv' else j-1
  jp1 = if j==nv' then 0 else j+1
  n1 = triangleNormal0 (vertices ! (i,j), vertices ! (i,jp1), vertices ! (ip1,j))
  n2 = triangleNormal0 (vertices ! (i,j), vertices ! (ip1,jm1), vertices ! (i,jm1))
  n3 = triangleNormal0 (vertices ! (i,j), vertices ! (im1,j), vertices ! (im1,jp1))
  n4 = triangleNormal0 (vertices ! (i,j), vertices ! (ip1,j), vertices ! (ip1,jm1))
  n5 = triangleNormal0 (vertices ! (i,j), vertices ! (i,jm1), vertices ! (im1,j))
  n6 = triangleNormal0 (vertices ! (i,j), vertices ! (im1,jp1), vertices ! (i,jp1))

allNormals :: Array (Int,Int) Point -> Array (Int,Int) Vector
allNormals vertices = array bounds associations
  where
  bounds = A.bounds vertices
  indices = A.indices vertices  
  g (i,j) = ((i,j), normalij vertices (i,j))
  associations = map g indices

trianglesij :: Array (Int,Int) Point -> Array (Int,Int) Vector 
            -> (Int, Int) -> (Int, Int)
            -> (NTriangle, NTriangle)
trianglesij vertices normals (nu,nv) (i,j) = 
  (((a,na), (b,nb), (c,nc)), ((c,nc), (b,nb), (d,nd)))
  where
  ip1 = if i==nu-1 then 0 else i+1
  jp1 = if j==nv-1 then 0 else j+1
  a = pointToVertex3 $ vertices ! (i,j)
  na = vectorToNormal3 $ normals ! (i,j)
  c = pointToVertex3 $ vertices ! (i,jp1)
  nc = vectorToNormal3 $ normals ! (i,jp1)
  d = pointToVertex3 $ vertices ! (ip1,jp1)
  nd = vectorToNormal3 $ normals ! (ip1,jp1)
  b = pointToVertex3 $ vertices ! (ip1,j)
  nb = vectorToNormal3 $ normals ! (ip1,j)

allTriangles :: (Int,Int) -> Double -> Double -> [(NTriangle,NTriangle)]
allTriangles nunv@(nu,nv) a nlobes =
  map (trianglesij vertices normals nunv) indices
  where
  vertices = allVertices (fun a nlobes) nunv
  normals = allNormals vertices
  indices = [(i,j) | i <- [0 .. nu-1], j <- [0 .. nv-1]]
