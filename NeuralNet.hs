module NeuralNet where

import Control.Monad
import Data.List
import System.Random

data Layer = Layer [[Float]] [Float]
  deriving (Eq, Read, Show)

data NN = NN [Layer]
  deriving (Eq, Read, Show)

numNeurons :: Layer -> Int
numNeurons (Layer _ thetas) = length thetas

numInputs :: Layer -> Int
numInputs (Layer (w:ws) _) = length w

validLayer :: Layer -> Bool
validLayer l@(Layer ws thetas) = all (== numInputs l) (map length ws) &&
  length ws == length thetas

validNN :: NN -> Bool
validNN (NN layers) = all validLayer layers &&
  and (zipWith (==) (init (map numNeurons layers))
    (tail (map numInputs layers)))

sigmoid = recip . (1 +) . exp . negate

applyLayer :: Layer -> [Float] -> [Float]
applyLayer (Layer ws thetas) input = zipWith f ws thetas
  where f ws theta = sigmoid $ theta + sum (zipWith (*) ws input)

-- | @applyLayers layers input@ applies the layers to the input
--   and returns a list of the results from each layer, final output first
applyLayers :: [Layer] -> [Float] -> [[Float]]
applyLayers layers input = reverse . scanl f input $ map applyLayer layers
  where f input g = g input

apply :: NN -> [Float] -> [Float]
apply (NN layers) = head . applyLayers layers
--apply (NN layers) input = foldl (flip applyLayer) input layers

randomLayer :: (Float, Float) -> Int -> Int -> IO Layer
randomLayer range i j = do
  weights <- replicateM j (replicateM i (randomRIO range))
  thetas <- replicateM j (randomRIO range)
  return $ Layer weights thetas

randomLayerD = randomLayer (-0.5, 0.5)

randomLayers :: [Int] -> IO [Layer]
randomLayers ns = sequence $ zipWith randomLayerD ns (tail ns)

zeroLayer i j = Layer (replicate j (replicate i 0)) (replicate j 0)

zeroLayerOf layer = zeroLayer (numInputs layer) (numNeurons layer)

zeroLayers :: [Int] -> [Layer]
zeroLayers ns = zipWith zeroLayer ns (tail ns)

randomNN :: [Int] -> IO NN
randomNN = liftM NN . randomLayers

neuronsPerLayer (NN layers) = numInputs (head layers) : map numNeurons layers

zeroNN :: [Int] -> NN
zeroNN = NN . zeroLayers

zeroNNOf = zeroNN . neuronsPerLayer

save :: NN -> FilePath -> IO ()
save nn filepath = writeFile filepath (show nn)

load :: FilePath -> IO NN
load filepath = readFile filepath >>= return . read

e :: [Float] -> [Float] -> Float
e target output = (/ 2) . sum $ map (** 2) $ zipWith (-) output target

dws_ eta alpha d is a o prevDws = zipWith (dw $ d a o) is prevDws
  where dw delta i prevDw = eta * delta * i + alpha * prevDw

dthetas_ eta alpha d a o prevDtheta = dtheta (d a o)
  where dtheta delta = eta * delta + alpha * prevDtheta

as_ d ws a o = sum $ map (* d a o) ws

data BackpropResult = BPR Layer [Float] Layer [([Float], [Float])]

backpropOutput :: Float -> Float -> BackpropResult -> BackpropResult
backpropOutput eta alpha (BPR (Layer ws thetas) ts (Layer prevDws prevDthetas)
  (os_is:nos_is)) = BPR (Layer newWs newThetas) as (Layer dws dthetas) nos_is
    where newWs = zipWith (zipWith (+)) ws dws
          dws = zipWith3 (dws_ eta alpha d is) ts os prevDws
          newThetas = zipWith (+) thetas dthetas
          dthetas = zipWith3 (dthetas_ eta alpha d) ts os prevDthetas
          d t o = (t - o) * o * (1 - o)
          as = zipWith3 (as_ d) ws ts os
          (os, is) = os_is

backpropHidden :: Float -> Float -> BackpropResult -> BackpropResult
backpropHidden eta alpha (BPR (Layer ws thetas) as (Layer prevDws prevDthetas)
  (os_is:nos_is)) = BPR (Layer newWs newThetas) nas (Layer dws dthetas) nos_is
    where newWs = zipWith (zipWith (+)) ws dws
          dws = zipWith3 (dws_ eta alpha d is) as os prevDws
          newThetas = zipWith (+) thetas dthetas
          dthetas = zipWith3 (dthetas_ eta alpha d) as os prevDthetas
          d a o = a * o * (1 - o)
          nas = zipWith3 (as_ d) ws as os
          (os, is) = os_is

extractLayers :: [BackpropResult] -> ([Layer], [Layer])
extractLayers [] = ([], [])
extractLayers (r@(BPR l _ dl _):rs) = (l:ls, dl:dls)
  where (ls, dls) = extractLayers rs

backpropFns eta alpha = backpropOutput eta alpha :
  repeat (backpropHidden eta alpha)

backprop :: Float -> Float -> NN -> NN -> [Float] -> [Float] -> (NN, NN)
backprop eta alpha nn@(NN layers) prevDnn@(NN prevDlayers) input target
  = (NN newLayers, NN dlayers)
    where (_:newLayers, _:dlayers) = extractLayers bprs
          outputs = applyLayers layers input
          os_is = zip outputs $ tail outputs
          bfns = backpropFns eta alpha
          backprops = zip3 bfns (reverse layers) (reverse prevDlayers)
          initR = BPR (zeroLayer 0 0) target (zeroLayer 0 0) os_is
          bprs = scanl f initR backprops
          f (BPR _ as _ os_is) (g, l, d) = g $ BPR l as d os_is

repeatBackprop eta alpha nn prevDnn input target 0 = (nn, prevDnn)
repeatBackprop eta alpha nn prevDnn input target times
  = repeatBackprop eta alpha nnn ndnn input target (times - 1)
    where (nnn, ndnn) = backprop eta alpha nn prevDnn input target

train :: Float -> Float -> NN -> [([Float], [Float])] -> Int -> NN
train _ _ nn [] _ = nn
train eta alpha nn pats@((input, target):rpats) timesPer
  = train eta alpha tnn rpats timesPer
    where (tnn, _) = repeatBackprop eta alpha nn (zeroNNOf nn) input target timesPer
