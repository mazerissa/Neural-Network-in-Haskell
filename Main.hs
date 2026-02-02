import System.Random (newStdGen, randomRs)
import Data.List (zipWith4)

data Layer = Layer {
    weights :: [[Double]], 
    biases  :: [Double]
} deriving (Show)

type Network = [Layer]

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Double -> Double
sigmoid' x = let s = sigmoid x in s * (1 - s)

dotProduct :: [Double] -> [Double] -> Double
dotProduct xs ys = sum $ zipWith (*) xs ys

createRandomNetwork :: [Int] -> IO Network
createRandomNetwork dims = do
    gen <- newStdGen
    let randoms = randomRs (-1.0, 1.0) gen
    return $ construct (length dims - 1) dims randoms
  where
    construct 0 _ _ = []
    construct n (i:j:rest) rs =
        let (wCount, bCount) = (j * i, j)
            (wRs, restRs) = splitAt wCount rs
            (bRs, finalRs) = splitAt bCount restRs
            ws = chunk i wRs 
            layer = Layer ws bRs
        in layer : construct (n-1) (j:rest) finalRs
    
    chunk _ [] = []
    chunk n xs = let (yh, yt) = splitAt n xs in yh : chunk n yt

forward :: [Double] -> Network -> [[Double]]
forward input [] = [input]
forward input (l:ls) = 
    let next = zipWith (\w b -> sigmoid (dotProduct w input + b)) (weights l) (biases l)
    in input : forward next ls

backprop :: [[Double]] -> [Double] -> Network -> [Layer]
backprop activations target network = go (reverse activations) (reverse network) []
  where
    go (out:hidden:as) (l:ls) [] =
        let delta = zipWith (*) (zipWith (-) out target) (map sigmoid' out)
            gradL = Layer [[d * h | h <- hidden] | d <- delta] delta
        in go (hidden:as) ls [gradL]
    go (act:prev:as) (l:ls) (nextGrad:gs) =
        let nextW = weights (head (nextGrad:gs))
            nextD = biases (head (nextGrad:gs))
            delta = zipWith (*) [dotProduct (map (!! i) nextW) nextD | i <- [0..length act - 1]] (map sigmoid' act)
            gradL = Layer [[d * p | p <- prev] | d <- delta] delta
        in go (prev:as) ls (gradL : nextGrad : gs)
    go _ _ gs = gs

updateLayer :: Double -> Layer -> Layer -> Layer
updateLayer lr (Layer w b) (Layer gw gb) =
    Layer (zipWith (zipWith (\w' g -> w' - lr * g)) w gw)
          (zipWith (\b' g -> b' - lr * g) b gb)

trainStep :: Double -> [Double] -> [Double] -> Network -> Network
trainStep lr input target net =
    let activations = forward input net
        gradients = backprop activations target net
    in zipWith (updateLayer lr) net gradients

trainDataset :: Double -> [([Double], [Double])] -> Network -> Network
trainDataset lr dataset net = foldl (\n (i, t) -> trainStep lr i t n) net dataset

main :: IO ()
main = do
    let architecture = [2, 4, 4, 3, 1]
    net <- createRandomNetwork architecture
    
    let xorData = [ ([0,0], [0]), ([0,1], [1]), ([1,0], [1]), ([1,1], [0]) ]
    let lr = 0.5
    let epochs = 5000
    
    let trainedNet = iterate (trainDataset lr xorData) net !! epochs
    
    putStrLn "Testing XOR results (Target: 0, 1, 1, 0):"
    mapM_ (\(inp, _) -> do
            let out = last (forward inp trainedNet)
            putStrLn $ show inp ++ " -> " ++ show out
          ) xorData