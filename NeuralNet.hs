module NeuralNet where

import Control.Monad
import System.Random

data Layer = Layer [[Float]] [Float]
  deriving (Eq, Read, Show)

data NN = NN [Layer]
  deriving (Eq, Read, Show)

numNeurons :: Layer -> Int
numNeurons (Layer _ thetas) = length thetas
--numNeurons2 (Layer ws _) = length ws

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
--apply2 (NN layers) input = foldl (flip applyLayer) input layers

randomLayer :: (Float, Float) -> Int -> Int -> IO Layer
randomLayer range i j = do
  weights <- replicateM j (replicateM i (randomRIO range))
  thetas <- replicateM j (randomRIO range)
  return $ Layer weights thetas

randomLayerD = randomLayer (-0.5, 0.5)

randomLayers :: [Int] -> IO [Layer]
randomLayers ns = sequence $ zipWith randomLayerD ns (tail ns)

randomNN :: [Int] -> IO NN
randomNN = liftM NN . randomLayers

save :: NN -> FilePath -> IO ()
save nn filepath = writeFile filepath (show nn)

load :: FilePath -> IO NN
load filepath = readFile filepath >>= return . read

e :: [Float] -> [Float] -> Float
e target output = (/ 2) . sum $ map (** 2) $ zipWith (-) output target

train :: NN -> [Float] -> [Float] -> NN
train nn input target = nn
--train nn input target = let output = apply nn input
--                            ds = zipWith d target output in
--  nn

backpropOutput :: Layer -> [Float] -> [Float] -> Float -> Float -> Layer -> (Layer, [Float])
backpropOutput hidden@(Layer ws thetas) outs trgs eta alpha prevD = (hidden, [0])
  where d t o = (t - o) * o * (1 - o)

backpropHidden :: Layer -> [Float] -> [Float] -> Float -> Float -> Layer -> (Layer, [Float])
backpropHidden hidden@(Layer ws thetas) outs adjs eta alpha prevD = (hidden, [0])
  where d a o = a * o * (1 - o)
