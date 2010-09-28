module Main where

import NeuralNet
import PGM
import System.IO

(eta, alpha) = (0.5, 0.9)

main = do
  inputs <- sequence [loadPGM "pgm/3backslash.pgm",
                      loadPGM "pgm/3circle.pgm",
                      loadPGM "pgm/3slash.pgm",
                      loadPGM "pgm/3x.pgm"]
  targets <- sequence [loadPGM "pgm/6backslash.pgm",
                       loadPGM "pgm/6circle.pgm",
                       loadPGM "pgm/6slash.pgm",
                       loadPGM "pgm/6x.pgm"]
  nn <- load "scaler.nn"
  putStr "Es: "
  let outputs = map (apply nn) inputs
  print $ zipWith e targets outputs

  putStr "Training... how many times per pattern? "
  hFlush stdout
  times <- readLn
  let nnn = train eta alpha nn (zip inputs targets) times

  putStr "New Es: "
  print $ zipWith e targets $ map (apply nnn) inputs

  putStr "Save (y/n)? "
  hFlush stdout
  i <- getChar
  if i == 'y' || i == 'Y'
    then save nnn "scaler.nn"
    else return ()
