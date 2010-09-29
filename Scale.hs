module Main where

import NeuralNet
import PGM
import System.Environment

dr [] _ _ = []
dr a b c = (PGM 3 3 $ a3 ++ b3 ++ c3) : dr ar br cr
  where (a3, b3, c3) = (take 3 a, take 3 b, take 3 c)
        (ar, br, cr) = (drop 3 a, drop 3 b, drop 3 c)

decompose :: PGM -> [PGM]
decompose (PGM _ _ []) = []
decompose (PGM w h pxs) = dr a b c ++ decompose (PGM w (h - 3) npxs)
  where rows = segment w pxs
        [a, b, c] = take 3 rows
        npxs = concat $ drop 3 rows

row :: Int -> [PGM] -> [Float]
row _ [] = []
row n ((PGM w h pxs):rpgms) = take w (drop (n*w) pxs) ++ row n rpgms

rows :: [PGM] -> [[Float]]
rows pgms@((PGM _ h _):rpgms) = [row n pgms | n <- [0..h]]

joinPGMs :: Int -> Int -> [PGM] -> PGM
joinPGMs nw nh pgms@((PGM w h pxs):rpgms) = PGM nw nh npxs
  where npxs = concat $ concatMap rows $ segment (nw `div` w) pgms

main = do
  nn <- load "scaler.nn"
  [pgmName, newName] <- getArgs
  pgm@(PGM w h _) <- loadPGM pgmName
  let t = map (PGM 6 6) $ map (apply nn . pxsPGM) (decompose pgm)
  let newPGM = joinPGMs (w*2) (h*2) t
  savePGM newName newPGM 255
