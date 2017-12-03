module Sudoku where

type Matrix a = [Row a]
type Row a = [a]

type Grid = Matrix Digit
type Digit = Char

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank = (=='0')

solve :: Grid -> [Grid]
solve = filter valid . expand . many prune . choices

completions :: Grid -> [Grid]
completions = expand . choices

valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs g)

type Choices = [Digit]

choices :: Grid -> Matrix Choices
choices = map (map choice)

choice :: Digit -> [Digit]
choice d = if blank d then digits else [d]

cp :: [[a]] -> [[a]]
cp []         = [[]]
cp (xs : xss) = [x : ys | x <- xs, ys <- yss]
  where
    yss = cp xss

expand :: Matrix Choices -> [Grid]
expand = cp . map cp

nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x : xs) = all (/= x) xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs: xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map group

group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs : group (drop 3 xs)

ungroup :: [[a]] -> [a]
ungroup = concat

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows

pruneBy :: ([Row Choices] -> [Row Choices]) -> [Row Choices] -> [Row Choices]
pruneBy f = f . map pruneRow . f

many :: (Eq a) => (a -> a) -> a -> a
many f x = if x == y then x else many f y
  where
    y = f x

pruneRow :: Row Choices -> Row Choices
pruneRow row = map (remove fixed) row
  where
    fixed = [d | [d] <- row]

remove :: Choices -> Choices -> Choices
remove ds [x] = [x]
remove ds xs  = filter (`notElem` ds) xs

example :: Grid
example = [ "004005700"
          , "000009400"
          , "360000008"
          , "720060000"
          , "000402000"
          , "000080093"
          , "400000056"
          , "005300000"
          , "006100900"
          ]
