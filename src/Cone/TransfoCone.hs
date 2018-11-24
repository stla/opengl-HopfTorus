module Cone.TransfoCone
  where
import           Data.Foldable (toList)
import           Linear
import           Control.Lens

transfoMatrixCone :: (Real a, Floating a) => a -> V3 a -> V3 a -> ([a], a)
transfoMatrixCone r2 cr1 cr2 = 
  (concatMap toList (toList (mkTransformationMat m trans)), height)
  where
    normal' = cr2 ^-^ cr1
    height = norm normal'
    trans = cr1 ^-^ cr2 -- V3 0 0 height
    normal = normal' ^/ height
    nx = normal ^. _x
    ny = normal ^. _y
    s = sqrt(nx*nx + ny*ny) -- TODO: case s=0
    u = V3 (ny/s) (-nx/s) 0
    v = cross normal u 
    m = transpose $ V3 u v normal