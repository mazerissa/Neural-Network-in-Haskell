module NeuralNet (
    Layer(..), 
    Network, 
    createRandomNetwork, 
    forward, 
    trainDataset, 
    trainStep, 
    updateLayer
) where
import System.Random (newStdGen, randomRs)

data Layer = Layer { weights :: [[Double]], biases :: [Double] } deriving (Show)
type Network = [Layer]

sigmoid x = 1 / (1 + exp (-x))
sigmoid' x = let s = sigmoid x in s * (1 - s)
dotProduct xs ys = sum $ zipWith (*) xs ys

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
            scale = sqrt (1.0 / fromIntegral i)
            ws = chunk i (map (* scale) wRs)
            layer = Layer ws (map (* 0) bRs)
        in layer : construct (n-1) (j:rest) finalRs
    chunk _ [] = []
    chunk n xs = let (yh, yt) = splitAt n xs in yh : chunk n yt

forward input [] = [input]
forward input (l:ls) = 
    let next = zipWith (\w b -> sigmoid (dotProduct w input + b)) (weights l) (biases l)
    in input : forward next ls

backprop activations target network = 
    let revActs = reverse activations
        revNet  = reverse network
        outDelta = zipWith (*) (zipWith (-) (head revActs) target) (map sigmoid' (head revActs))
        allDeltas = scanl calculateHiddenDelta outDelta (zip revNet (tail revActs))
        calculateHiddenDelta nextDelta (Layer w _, act) =
            let errors = [dotProduct (map (!! i) w) nextDelta | i <- [0..length act - 1]]
            in zipWith (*) errors (map sigmoid' act)
        grads = zipWith (\act d -> Layer [[d' * a | a <- act] | d' <- d] d) (tail revActs) allDeltas
    in reverse grads

updateLayer lr (Layer w b) (Layer gw gb) =
    Layer (zipWith (zipWith (\w' g -> w' - lr * g)) w gw)
          (zipWith (\b' g -> b' - lr * g) b gb)

trainStep lr input target net =
    let activations = forward input net
        gradients = backprop activations target net
    in zipWith (updateLayer lr) net gradients

trainDataset lr dataset net = foldl (\n (i, t) -> trainStep lr i t n) net dataset