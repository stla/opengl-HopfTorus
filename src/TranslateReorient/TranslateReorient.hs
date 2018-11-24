module TranslateReorient.TranslateReorient
  where
import           Data.Foldable (toList)
import           Linear

translateAndReorient :: (Real a, Floating a) => V3 a -> V3 a -> [a]
translateAndReorient axis vector = 
  concatMap toList (toList (mkTransformationMat m vector))
  where
    vx1 = axis ^/ (norm axis)
    vx2 = vector ^/ (norm vector)
    y' = cross vx1 vx2
    y = y' ^/ norm y'
    z1' = cross vx1 y
    z1 = z1' ^/ norm z1'
    z2' = cross vx2 y
    z2 = z2' ^/ norm z2'
    m1 = transpose $ V3 vx1 y z1
    m2 = V3 vx2 y z2
    m = transpose $ m1 !*! m2