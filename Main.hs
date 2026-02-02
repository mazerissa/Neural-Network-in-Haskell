import Graphics.Gloss
import NeuralNet

nodeRadius = 15
layerGap   = 160
nodeGap    = 40
screenOff  = -320 

xorData = [ ([0,0], [0]), ([0,1], [1]), ([1,0], [1]), ([1,1], [0]) ]
learningRate = 0.5

weightColor w = 
    let intensity = realToFrac (min 1.0 (abs w))
    in if w > 0 
       then mixColors intensity (1 - intensity) cyan (greyN 0.1)
       else mixColors intensity (1 - intensity) orange (greyN 0.1)

getPos lIdx nIdx totalNodes = 
    ( fromIntegral lIdx * layerGap + screenOff
    , fromIntegral nIdx * nodeGap - (fromIntegral totalNodes * nodeGap / 2)
    )

drawConnections lIdx layer prevTotal =
    let nextTotal = length (biases layer)
    in pictures [ 
        let w = (weights layer !! j) !! i
            (x1, y1) = getPos lIdx i prevTotal
            (x2, y2) = getPos (lIdx+1) j nextTotal
        in color (weightColor w) $ line [(x1, y1), (x2, y2)]
        | i <- [0..prevTotal-1], j <- [0..nextTotal-1] 
    ]

render net = 
    let architecture = [2, 8, 8, 4, 1]
        prevLayerCounts = init architecture 
        netColors = pictures $ zipWith3 drawConnections [0..] net prevLayerCounts
        nodes = pictures $ zipWith (\l t -> pictures [ translate x y $ color white (circleSolid nodeRadius) 
                                                     | i <- [0..t-1], let (x, y) = getPos l i t ]) [0..] architecture
    in pictures [netColors, nodes]

update _ _ net = 
    iterate (trainDataset learningRate xorData) net !! 10

main :: IO ()
main = do
    initialNet <- createRandomNetwork [2, 8, 8, 4, 1]
    
    simulate 
        (InWindow "Neural Network In Haskell" (800, 600) (100, 100))
        black        
        60           
        initialNet   
        render      
        update       