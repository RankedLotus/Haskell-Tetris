{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
import CodeWorld
import qualified Data.Text as T
import System.Random
import Data.List
main = do
       gen1 <- newStdGen
       gen2 <- newStdGen
       let start = Game 0.0 (plist !! ( fst $ (randomR (0, 6) gen1))) [] 0 gen2
       print "Hello World"
       activityOf start evh draw
--Piece Structure
data Piece = Piece { c :: Color,
                     og :: Point,
                     q :: [Point],
                     count :: Double,
                     ident :: Int}
--Game Structure
data Block = Block { pos :: Point,
                     col :: Color}
data Game = Game { timer :: Double,
                   actpiece :: Piece,
                   piecelist :: [Block],
                   score :: Int,
                   gen :: StdGen}
--Piece List
cyan :: Color
cyan = RGB 0.0 1.0 1.0
sqpiece = Piece yellow (0, 9) [(1, 0), (1, 1), (0, 1)] 0.0 1
tpiece = Piece purple (0, 9) [(-1, 0), (0, 1), (1, 0)] 0.0 0
ipiece = Piece cyan (0.5, 8.5) [(-0.5, 0.5), (-0.5, 1.5), (-0.5, -0.5), (-0.5, -1.5)] 0.0 2
zpiece = Piece red (0, 9) [(-1, 1), (0, 1), (1, 0)] 0.0 0
spiece = Piece green (0, 9) [(-1, 0), (0, 1), (1, 1)] 0.0 0
lpiece = Piece orange (0, 9) [(0, -1), (0, 1), (1, -1)] 0.0 0
jpiece = Piece blue (0, 9) [(0, -1), (0, 1), (-1, -1)] 0.0 0
plist = [sqpiece, tpiece, ipiece, zpiece, spiece, lpiece, jpiece]
--Draw Handler
bmaker pl = [translated x y $ colored c $ solidRectangle 1 1 | (Block (x, y) c) <- pl]
draw' g@(Game timer (Piece c (x,y) q count ident) piecelist score gen)
  | x == 100 = pictures $ (translated 0 4 $ colored white $ scaled 3 3 $ lettering "You Lose!") :
                          (translated (-8) 8 $ colored white $ lettering $ T.pack $ take 4 $ show timer) : (conv c (x, y) q) ++ (bmaker piecelist)
  | ident == 2 = pictures $ (translated (-8) 8 $ colored white $ lettering $ T.pack $ take 4 $ show timer) : (conv c (x, y) q) ++ 
                         (conv grey (nx, ny) nq) ++ (bmaker piecelist)
  | otherwise = pictures $ (translated (-8) 8 $ colored white $ lettering $ T.pack $ take 4 $ show timer) : 
                         (translated x y $ colored c $ solidRectangle 1 1) : (conv c (x, y) q) ++ 
                         (translated nx ny $ colored grey $ solidRectangle 1 1) : (conv grey (nx, ny) nq)
                         ++ (bmaker piecelist)
  where (Game nt npiece@(Piece nc (nx, ny) nq ncount nident) npc ns ngen) = harddrop g
draw g = pictures [grid, draw' g, colored black $ solidRectangle 20 20]
       
conv col (x0,y0) ((x,y):pts) = (translated (x+x0) (y+y0) $ colored col $ solidRectangle 1 1): conv col (x0,y0) pts
conv _ _ [] = []
--Limiter
limit :: Piece -> Piece
limit (Piece c (x, y) q count ident)
  | (y + bottom) < -9 = Piece c (x, (-9 - bottom)) q count ident
  | (x + left) < -5 = Piece c ((-5 - left), y) q count ident
  | (x + right) > 5 = Piece c ((5 - right), y) q count ident
  | otherwise = Piece c (x, y) q count ident
  where left = fst $ foldl min (4, 4) q
        right = fst $ foldl max (-4, -4) q
        bottom = snd $ head $ sortOn snd q
--evh helpers
rotate qs = [(y, -1 * x) | (x, y) <- qs]
interacts :: Piece -> [Block] -> Bool
interacts (Piece col (x, y) ((a, b):(c, d):(e, f):xs) count ident) pc
  | (ident /= 2) &&
    ((a + x, b + y - 1) `elem` poslist || 
    (c + x, d + y - 1) `elem` poslist ||
    (e + x, f + y - 1) `elem` poslist ||
    (x, y - 1) `elem` poslist) = True
  | (xs /= []) && 
    ((a + x, b + y - 1) `elem` poslist || 
    (c + x, d + y - 1) `elem` poslist ||
    (e + x, f + y - 1) `elem` poslist ||
    (x + (fst $ head $ xs), y + (snd $ head $ xs) - 1) `elem` poslist) = True
  | otherwise = False
  where poslist = [(m, n) | (Block (m, n) c) <- pc]
interactsb :: Piece -> [Block] -> Bool
interactsb (Piece c (x, y) q count ident) pc
  | -9 == y + b = True
  | otherwise = False
  where bottom@(a, b) = head $ sortOn snd q
interactsc :: Piece -> [Block] -> Bool
interactsc (Piece c (x, y) q count ident) pc
  | True `elem` [(x + a + 1, y + b) `elem` pcs | (a, b) <- (0, 0):q] = True
  | otherwise = False
  where pcs = [(x, y) | (Block (x, y) col) <- pc]
interactsd :: Piece -> [Block] -> Bool
interactsd (Piece c (x, y) q count ident) pc
  | True `elem` [(x + a - 1, y + b) `elem` pcs | (a, b) <- (0, 0):q] = True
  | otherwise = False
  where pcs = [(x, y) | (Block (x, y) col) <- pc]
--Line clearing:
clrln' n pc
  | length nlist == 11 = [Block (a, b - 1) c | (Block (a, b) c) <- pc, b > n] ++ [Block (a, b) c | (Block (a, b) c) <- pc, b < n]
  | otherwise = pc
  where nlist = [(a, b) | (Block (a, b) c) <- pc, b == n]
clrln pc 8 = pc
clrln pc n = clrln (clrln' n pc) (n + 1)
--Harddrop function
harddrop (Game t piece@(Piece c (x, y) q count ident) pc s gen)
  | interacts piece pc || interactsb piece pc = Game t piece pc s gen
  | otherwise = harddrop (Game t (Piece c (x, y - 1) q count ident) pc s gen)
--EVH
evh :: Event -> Game -> Game
evh (TimePassing t) (Game time piece@(Piece c (x,y) q count ident) pc s gen)
  | 8 `elem` [ y | (Block (x, y) col) <- pc] = Game time (Piece c (100, 100) q count ident) pc s gen'
  | interacts piece pc && ident == 2 && (fromIntegral (round count) `rem` 2) == 1 = Game (time + 0.02) (plist !! npi) ([Block (x + a, y + b) c | (a, b) <- q] ++ pc)
                                                                s gen'
  | interacts piece pc && (fromIntegral (round count) `rem` 2) == 1 = Game time (plist !! npi) ((Block (x, y) c) : 
                                                                [Block (x + a, y + b) c | (a, b) <- q] ++ pc)
                                                                s gen'
  | interactsb piece pc && ident == 2 && (fromIntegral (round count) `rem` 2) == 1 = Game time (plist !! npi) ([Block (x + a, y + b) c | (a, b) <- q] ++ pc)
                                                                 s gen'
  | interactsb piece pc && (fromIntegral (round count) `rem` 2) == 1 = Game time (plist !! npi) ((Block (x, y) c) : 
                                                                [Block (x + a, y + b) c | (a, b) <- q] ++ pc)
                                                                s gen'
  | (fromIntegral (round count) `rem` 2) == 1 = Game (t + time) (limit $ Piece c (x, (y - 1)) q (count + 1) ident) pc s gen
  | otherwise = Game (t + time) (limit $ Piece c (x, y) q (count + 0.02) ident) (clrln pc (-9)) s gen
  where (npi, gen') = randomR (0, 6) gen :: (Int, StdGen)
--Rotation
evh (KeyPress "R") (Game t (Piece c og q count ident) pc s gen) = (Game 0 (Piece c og q count ident) [] 0 gen)

evh (KeyPress "Up") (Game t piece@(Piece c og q count ident) pc s gen)
  | ident == 1 && (not $ interactsc piece pc) = Game t (Piece c og q count ident) pc s gen
  | not $ interactsc piece pc = Game t (limit $ Piece c og (rotate q) count ident) pc s gen
  | otherwise = Game t (limit $ Piece c og (rotate $ rotate $ rotate q) count ident) pc s gen
--Soft Drop
evh (KeyPress "Down") (Game t (Piece c og q count ident) pc s gen) = Game t (limit $ Piece c og q (count + 1) ident) pc s gen
--Movement
evh (KeyPress "Left") (Game t piece@(Piece c (x,y) q count ident) pc s gen)
  | not $ interactsd piece pc = Game t (limit $ Piece c (x - 1, y) q count ident) pc s gen
  | otherwise = Game t piece pc s gen
evh (KeyPress "Right") (Game t piece@(Piece c (x,y) q count ident) pc s gen)
  | not $ interactsc piece pc = Game t (limit $ Piece c (x + 1, y) q count ident) pc s gen
  | otherwise = Game t piece pc s gen
evh (KeyPress " ") game = harddrop game
evh _ (Game t p pc s g) = (Game t (limit p) pc s g)
--Grid art
grid = pictures [translated x y $ colored white $ thickRectangle 0.06 1 1 | x <- [-5.. 5], y <- [-9.. 7]]

