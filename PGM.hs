module PGM where

dropComments = filter (\(s:ss) -> s /= '#')

loadPGM :: FilePath -> IO [Float]
loadPGM file = do
  text <- readFile file
  let noComments = dropComments $ lines text
  let maxG = read $ noComments !! 2
  return $ map ((/maxG).read) $ concatMap words $ drop 3 noComments
