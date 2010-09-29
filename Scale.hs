module Main where

import NeuralNet
import PGM
import System.Environment

tileRow :: [Float] -> [Float] -> [Float] -> [PGM]
tileRow [] _ _ = []
tileRow a b c = (PGM 3 3 $ a3 ++ b3 ++ c3) : tileRow ar br cr
  where (a3, b3, c3) = (take 3 a, take 3 b, take 3 c)
        (ar, br, cr) = (drop 3 a, drop 3 b, drop 3 c)

tile :: PGM -> [PGM]
tile (PGM _ _ []) = []
tile (PGM w h pxs) = tileRow a b c ++ tile (PGM w (h - 3) npxs)
  where lines = segment w pxs
        [a, b, c] = take 3 lines
        npxs = concat $ drop 3 lines

row :: Int -> [PGM] -> [Float]
row _ [] = []
row n ((PGM w h pxs):rpgms) = take w (drop (n*w) pxs) ++ row n rpgms

pixelRows :: [PGM] -> [[Float]]
pixelRows pgms@((PGM _ h _):rpgms) = [row n pgms | n <- [0..h]]

joinPGMs :: Int -> Int -> [PGM] -> PGM
joinPGMs nw nh pgms@((PGM w h pxs):rpgms) = PGM nw nh npxs
  where npxs = concat $ concatMap pixelRows $ segment (nw `div` w) pgms

main = do
  nn <- load "scaler.nn"
  [pgmName, newName] <- getArgs
  pgm@(PGM w h _) <- loadPGM pgmName
  let scaledTiles = map (PGM 6 6) $ map (apply nn . pgmPixels) (tile pgm)
  let newPGM = joinPGMs (w*2) (h*2) scaledTiles
  savePGM newName newPGM 255
