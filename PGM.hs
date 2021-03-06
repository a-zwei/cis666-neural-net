module PGM where

data PGM = PGM Int Int [Float]

dropComments = filter (\(s:ss) -> s /= '#')

pgmPixels (PGM _ _ pxs) = pxs

loadPGM :: FilePath -> IO PGM
loadPGM file = do
  text <- readFile file
  let noComments = dropComments $ lines text
  let [width, height] = map read $ words $ noComments !! 1
  let maxG = read $ noComments !! 2
  return $ PGM width height $
    map ((/ maxG) . read) $ concatMap words $ drop 3 noComments

segment :: Int -> [a] -> [[a]]
segment _ [] = []
segment n xs = f : segment n fs
  where (f, fs) = splitAt n xs

savePGM :: FilePath -> PGM -> Int -> IO ()
savePGM file (PGM w h pxs) maxG = do
  let dim = show w ++ " " ++ show h
  let segments = segment w $ map (show . round . (* fromIntegral maxG)) pxs 
  writeFile file $ "P2\n" ++ dim ++ "\n" ++ show maxG ++ "\n"
  appendFile file $ unlines $ map unwords $ segments
