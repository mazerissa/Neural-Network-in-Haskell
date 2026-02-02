import Graphics.Gloss
import NeuralNet

nodeRadius = 15
layerGap   = 160
nodeGap    = 40
screenOff  = -320 

xorData = [ ([0,0], [0]), ([0,1], [1]), ([1,0], [1]), ([1,1], [0]) ]
learningRate = 0.5

data State = State { net :: Network, history :: [Float], time :: Float }

weightColor w = 
    let intensity = realToFrac (min 1.0 (abs w))
    in if w > 0 
       then mixColors intensity (1 - intensity) (light cyan) (greyN 0.1)
       else mixColors intensity (1 - intensity) (light orange) (greyN 0.1)

getPos lIdx nIdx totalNodes = 
    ( fromIntegral lIdx * layerGap + screenOff
    , fromIntegral nIdx * nodeGap - (fromIntegral totalNodes * nodeGap / 2)
    )

drawSpark p1 p2 t =
    let progress = mod' t 1.0
        (x1, y1) = p1
        (x2, y2) = p2
        sx = x1 + (x2 - x1) * progress
        sy = y1 + (y2 - y1) * progress
    in translate sx sy $ color white $ circleSolid 3

render (State currentNet errs t) = pictures [connections, sparks, nodes, graph, labels]
  where
    architecture = [2, 8, 8, 4, 1]
    prevCounts = init architecture
    
    connections = pictures $ zipWith3 drawLayer [0..] currentNet prevCounts
    drawLayer lIdx layer prevT = pictures 
        [ color (weightColor ((weights layer !! j) !! i)) $ 
          line [getPos lIdx i prevT, getPos (lIdx+1) j (length (biases layer))]
        | i <- [0..prevT-1], j <- [0..length (biases layer)-1] ]

    sparks = pictures $ zipWith3 drawLayerSparks [0..] currentNet prevCounts
    drawLayerSparks lIdx layer prevT = pictures
        [ drawSpark (getPos lIdx i prevT) (getPos (lIdx+1) j (length (biases layer))) (t + fromIntegral (i+j))
        | i <- [0..prevT-1], j <- [0..length (biases layer)-1]
        , abs ((weights layer !! j) !! i) > 0.5 ]

    nodes = pictures $ zipWith (\l t -> pictures 
        [ translate x y $ pictures [color white (circleSolid nodeRadius), color black (circle nodeRadius)]
        | i <- [0..t-1], let (x, y) = getPos l i t ]) [0..] architecture

    graph = translate (-350) (-250) $ color green $ line $ 
            zip [0, 2..] (take 350 $ map (* 200) errs)

    labels = translate (-350) 280 $ scale 0.12 0.12 $ color white $ 
             text $ "Loss: " ++ show (if null errs then 0 else head errs)

update _ dt (State currentNet errs t) = 
    let nextNet = iterate (trainDataset learningRate xorData) currentNet !! 10
        currentError = sum [ (head (last (forward inp nextNet)) - head target) ** 2 
                           | (inp, target) <- xorData ] / 4
    in State nextNet (realToFrac currentError : take 500 errs) (t + dt)

mod' x y = x - fromIntegral (floor (x / y)) * y

main :: IO ()
main = do
    
    initialNet <- createRandomNetwork [2, 8, 8, 4, 1]
    simulate 
        (InWindow "Neural Net Live Brain" (900, 700) (50, 50))
        (greyN 0.05) 60 (State initialNet [] 0) render update