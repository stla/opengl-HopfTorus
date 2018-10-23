module Utils.Normals
  where
import           Graphics.Rendering.OpenGL.GL

triangleNormal :: Floating a => (Vertex3 a, Vertex3 a, Vertex3 a) -> Normal3 a
triangleNormal (Vertex3 x1 x2 x3, Vertex3 y1 y2 y3, Vertex3 z1 z2 z3) =
  Normal3 (a/norm) (b/norm) (c/norm)
  where
    (a, b, c) = crossProd (y1-x1, y2-x2, y3-x3) (z1-x1, z2-x2, z3-x3)
    crossProd (a1,a2,a3) (b1,b2,b3) = (a2*b3-a3*b2, a3*b1-a1*b3, a1*b2-a2*b1)
    norm = sqrt (a*a + b*b + c*c)

negateNormal :: Floating a => Normal3 a -> Normal3 a
negateNormal (Normal3 x y z) = Normal3 (-x) (-y) (-z)
