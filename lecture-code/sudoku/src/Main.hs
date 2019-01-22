module Main where

main :: IO ()
main = do
  putStrLn "hello world"

-- function composition


-- define type synonyms

type Matrix a = [[a]]
type Board = Matrix Char

testBoard :: Board
testBoard = ["2....1.38",
              "........5",
              ".7...6...",
              ".......13",
              ".981..257",
              "31....8..",
              "9..8...2.",
              ".5..69784",
              "4..25...."]

closeBoard :: Board
closeBoard = ["4.3921657",
             "967345821",
             "25.876493",
             "548.32976",
             "72956.138",
             "136798245",
             "372.89514",
             "8142537.9",
             "695417382"]


fullBoard :: Board
fullBoard = ["483921657",
             "967345821",
             "251876493",
             "548132976",
             "729564138",
             "136798245",
             "372689514",
             "814253769",
             "695417382"]



-- goal -- sudoku :: Board -> [Board]

boardsize = 9
boxsize = 3
cellvals = ['1'..'9']
blank = (=='.')

--------------
-- check board
--------------

correct :: Board -> Bool
correct b = all nodups (rows b) &&
            all nodups (cols b) &&
            all nodups (boxs b)

rows :: Matrix a -> Matrix a
rows = id

-- rows . rows = id

cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

-- cols . cols = id  (identity on matrices)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map group

-- since
--     group . ungroup = id
--     ungroup . group = id
--     map f . map g = map (f . g)
--     map id = id
-- we have
--     boxs . boxs = (map ungroup . ungroup . map cols . group . map group) . 
--                   (map ungroup . ungroup . map cols . group . map group
--                 = ...
--                 = id


nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = notElem x xs && nodups xs


-------------------------
-- initial implementation
-------------------------

type Choices = [Char]

choices :: Board -> Matrix Choices
choices = map (map choose)
  where choose e = if blank e then cellvals else [e]

-- matrix cartesian product
mcp :: Matrix [a] -> [Matrix a]
mcp = cp . map cp
  where cp [] = [[]]
        cp (xs:xss) = [x : ys | x <- xs, ys <- cp xss]

-- > mcp [["1", "23"], ["4","5"]] !! 0

sudoku :: Board -> [Board]
sudoku = filter correct . mcp . choices

-- sudoku b = filter correct $ mcp $ choices b

-- 9^40 boards to check! (assuming half full)


-------
-- v2.0
-------

-- prune choices, goal `prune` function s.t. 
--       filter correct . mcp = filter correct . mcp . prune

fixed :: [Choices] -> Choices
fixed = concat . filter single

reduce :: [Choices] -> [Choices]
reduce css = map (remove (fixed css)) css
  where remove fs cs = if single cs then cs else delete fs cs

pruneBy :: (Matrix Choices -> Matrix Choices) ->
           (Matrix Choices -> Matrix Choices)
pruneBy f = f . map reduce . f

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows

sudoku' :: Board -> [Board]
sudoku' = filter correct . mcp . prune . choices


-------
-- v3.0
-------

blocked :: Matrix Choices -> Bool
blocked cm = void cm || not (safe cm)

void :: Matrix Choices -> Bool
void = any (any null)

safe :: Matrix Choices -> Bool
safe cm = all (nodups . fixed) (rows cm) &&
          all (nodups . fixed) (cols cm) &&
          all (nodups . fixed) (boxs cm)

expand cm = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where (rows1, row : rows2) = break (any best) cm
        (row1, cs : row2) = break best row
        best cs = (length cs == n)
        n = minchoice cm
        minchoice = minimum . filter (> 1) . concat . map (map length)

search :: Matrix Choices -> [Matrix Choices]
search cm
  | blocked cm = []
  | all (all single) cm = [cm]
  | otherwise = (concat . map (search . prune) . expand) cm


sudoku'' :: Board -> [Board]
sudoku'' = map (map (map head)) . search . prune . choices




----------
-- helpers
----------
  
group :: [a] -> [[a]]
group = groupBy boxsize

ungroup :: [[a]] -> [a]
ungroup = concat

groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy  n xs = as : groupBy n bs 
  where (as,bs) = splitAt n xs


single :: [a] -> Bool
single (x:[]) = True
single _ = False

delete :: (Eq a) => [a] -> [a] -> [a]
delete ds = filter $ not . flip elem ds
