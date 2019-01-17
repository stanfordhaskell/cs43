module Main where

main :: IO ()
main = do
  putStrLn "hello world"

-- Source: Composing fractals, Mark P. Jones
-- http://web.cecs.pdx.edu/~mpj/pubs/composing-fractals.pdf
--
-- Why is this cool?
-- A concise, elegant implementation of Mandelbrot
-- Emphasis on higher order functions, not recursion

type Point = (Float, Float)

-- How to tell if point p is in Mandelbrot set?
-- 1) Construct a sequence of points, mandelbrot p
-- 2) If points are "fairly close" to origin, then conclude that p is in Mandelbrot set
-- 3) If the points "diverge", i.e. get further and further away, then p is not in Mandelbrot set

next :: Point -> Point -> Point
next (u, v) (x, y) = (x*x - y*y + u, 2*x*y + v)

mandelbrot :: Point -> [Point]
mandelbrot p = iterate (next p) (0, 0)

-- Try a few values, sample from mandelbrot now
-- (0.5, 0)
-- (0.1, 0)
--

-- An approximation: how do we define "fairly close" to the origin?
fairlyClose :: Point -> Bool
fairlyClose (u, v) = (u * u + v * v) < 100

-- This could be infinite: not a computable function
inMandelbrotSet :: Point -> Bool
inMandelbrotSet p = all fairlyClose (mandelbrot p)

approxTest :: Int -> Point -> Bool
approxTest n p = all fairlyClose (take n (mandelbrot p))

-- Now, onto rendering.

-- This definition is polymorphic
-- "color" is a type variable, can mean different types in different settings.
-- Choose a oclor based on how many points are "fairlyClose" to the origin.
chooseColor :: [color] -> [Point] -> color
chooseColor palette = (palette !!) . length . take n . takeWhile fairlyClose
    where n = length palette - 1

-- An image is a mapping that assigns a color value to each point.
-- Mandelbrot set is a value of type Image Bool
-- Points in the set are True, points outside are false.
type Image color = Point -> color

-- Fractal, like mandelbrot, takes Point -> [Point]
fracImage :: (Point -> [Point]) -> [color] -> Image color
fracImage fractal palette = chooseColor palette . fractal

-- Logic to model the grid
-- Grids are represented as lists of lists
type Grid a = [[a]]

-- Construct a grid.  c: num of columns, r: num of rows.
grid :: Int -> Int -> Point -> Point -> Grid Point

grid c r (xmin, ymin) (xmax, ymax)
    = [[ (x, y) | x <- for c xmin xmax] | y <- for r ymin ymax]

-- Pick c evenly spaced values for x in the range xmin to xmax
-- Pick r evenly spaced values for y in the range ymin to ymax
-- Subtlety: need fromIntegral to convert the integral so it can
-- be used for floating point arithmetic.
for :: Int -> Float -> Float -> [Float]
for n min max = take n [min, min+delta ..]
    where delta = (max - min) / fromIntegral (n - 1)

-- Give a grid point, sample the image at each position
-- to obtain a grid of colors that is ready for display.
-- Nested calls to map to iterate over the list of lists in the input grid.
sample :: Grid Point -> Image color -> Grid color
sample points image = map (map image) points

draw points fractal palette render
    = render (sample points (fracImage fractal palette))

-- Draw is polymorphic in the type of image produced
draw :: Grid Point -> (Point -> [Point])
                   -> [color]
                   -> (Grid color -> image)
                   -> image

-------------------------------
-- Character based rendering -- 
-------------------------------
charPalette :: [Char]
charPalette = " ,.‘\"~:;o-!|?/<>X+={^O#%&@8*$"

charRender :: Grid Char -> IO ()
charRender = putStr . unlines

-- Mandelbrot! --
figure1 = draw points mandelbrot charPalette charRender
    where points = grid 79 37 (-2.25, -1.5) (0.75, 1.5)

-- Julia! --

julia :: Point -> Point -> [Point]
julia c = iterate (next c)

figure2 = draw points (julia (0.32, 0.043)) charPalette charRender
    where points = grid 79 37 (-1.5, -1.5) (1.5, 1.5)


