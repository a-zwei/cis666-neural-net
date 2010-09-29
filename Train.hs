module Main where

import NeuralNet
import PGM
import System.Environment
import System.IO

(eta, alpha) = (0.5, 0.9)

main = do
  ipgms <- sequence [loadPGM "pgm/3backslash.pgm",
                     loadPGM "pgm/3circle.pgm",
                     loadPGM "pgm/3slash.pgm",
                     loadPGM "pgm/3x.pgm",
                     loadPGM "pgm/3backslashI.pgm",
                     loadPGM "pgm/3circleI.pgm",
                     loadPGM "pgm/3slashI.pgm",
                     loadPGM "pgm/3xI.pgm",
                     loadPGM "pgm/3white.pgm",
                     loadPGM "pgm/3black.pgm"]
  tpgms <- sequence [loadPGM "pgm/6backslash.pgm",
                     loadPGM "pgm/6circle.pgm",
                     loadPGM "pgm/6slash.pgm",
                     loadPGM "pgm/6x.pgm",
                     loadPGM "pgm/6backslashI.pgm",
                     loadPGM "pgm/6circleI.pgm",
                     loadPGM "pgm/6slashI.pgm",
                     loadPGM "pgm/6xI.pgm",
                     loadPGM "pgm/6white.pgm",
                     loadPGM "pgm/6black.pgm"]

  let inputs = map (\(PGM _ _ pxs) -> pxs) ipgms
  let targets = map (\(PGM _ _ pxs) -> pxs) tpgms

  [nnFile] <- getArgs
  nn <- load nnFile

  putStr "Es: "
  print $ zipWith e targets $ map (apply nn) inputs

  putStr "Training... how many times per pattern? "
  hFlush stdout
  timesPer <- readLn
  putStr "how many times? "
  hFlush stdout
  times <- readLn
  let nnn = trainCycle eta alpha nn (zip inputs targets) times timesPer

  putStr "New Es: "
  print $ zipWith e targets $ map (apply nnn) inputs

  putStr "Save (y/n)? "
  hFlush stdout
  i <- getChar
  if i == 'y' || i == 'Y'
    then save nnn nnFile
    else putStrLn "Exiting without saving!"
