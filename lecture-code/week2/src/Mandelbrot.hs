import Data.Complex

-- Adapted from http://gitcommit.co.uk/2017/03/25/simple-fractals-in-haskell/
type C = Complex Double
type Pnt = (Double, Double)
type Grid a = [[a]]

mandel :: C -> [C]
mandel c = iterate (\z -> z * z + c) (0.0 :+ 0.0)

 {-a grid-}
{-grid1 :: Grid Pnt-}
{-grid1 = grid w h (-1.5, -1.5) (1.5, 1.5)-}

 {-used by juicy pixels to get a pixel value at x, y-}
{-gridFunc ::Grid Pnt ->  (C -> [C]) -> Int -> Int -> PixelRGB8-}
{-gridFunc grd fz  x y = ((mapOverGrid grd fz rgb8) !! y) !! x-}

{-generateImg :: (Int -> Int -> PixelRGB8) -> DynamicImage-}
{-generateImg gf = ImageRGB8 (generateImage gf w h)-}

main :: IO()
main = do
  print "Hello"

