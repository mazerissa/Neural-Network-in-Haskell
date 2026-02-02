import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import NeuralNet
import Data.Fixed (mod')

rad = 15
gapX = 160
gapY = 40
offX = -320 

dat = [ ([0,0], [0]), ([0,1], [1]), ([1,0], [1]), ([1,1], [0]) ]
lr = 0.5 
arch = [2, 8, 8, 4, 1]

data State = State { net :: Network, history :: [Float], t :: Float }

c w = 
    let i = realToFrac (min 1.0 (abs w))
    in if w > 0 
       then mixColors i (1-i) cyan (greyN 0.1)
       else mixColors i (1-i) orange (greyN 0.1)

pos l n total = 
    ( fromIntegral l * gapX + offX
    , fromIntegral n * gapY - (fromIntegral total * gapY / 2)
    )

render (State n h time) = pictures [wlines, sparks, nodes, plot, txt, results]
  where
    prevs = init arch

    wlines = pictures $ zipWith3 (\lIdx layer pCount -> 
        pictures [ color (c ((weights layer !! j) !! i)) $ 
                   line [pos lIdx i pCount, pos (lIdx+1) j (length (biases layer))]
                 | i <- [0..pCount-1], j <- [0..length (biases layer)-1] ]) [0..] n prevs

    sparks = pictures $ zipWith3 (\lIdx layer pCount ->
        pictures [ let p1 = pos lIdx i pCount
                       p2 = pos (lIdx+1) j (length (biases layer))
                       prog = mod' (time * 0.7 + fromIntegral (i+j)) 1.0 
                       (x1,y1) = p1
                       (x2,y2) = p2
                   in translate (x1+(x2-x1)*prog) (y1+(y2-y1)*prog) $ color white $ circleSolid 2
                 | i <- [0..pCount-1], j <- [0..length (biases layer)-1]
                 , abs ((weights layer !! j) !! i) > 0.8 ]) [0..] n prevs

    nodes = pictures $ zipWith (\l t -> pictures 
        [ translate x y $ pictures [color (greyN 0.8) (circleSolid rad), color white (circle (rad+2))]
        | i <- [0..t-1], let (x, y) = pos l i t ]) [0..] arch

    plot = translate (-380) (-300) $ color (dim green) $ line $ zip [0, 2..] (map (*150) (take 400 h))

    txt = translate (-440) 330 $ scale 0.12 0.12 $ color white $ pictures
          [ text $ "LOSS: " ++ show (if null h then 1.0 else head h)
          , translate 0 (-200) $ text "Hit R to Reset the NN" 
          ]

    results = translate 200 330 $ scale 0.12 0.12 $ color (light (light cyan)) $ pictures $
        zipWith (\(i, _) idx -> translate 0 (fromIntegral idx * (-180)) $ 
            text $ show i ++ " -> " ++ show (head (last (forward i n)))) dat [0..3]

handle (EventKey (Char 'r') Down _ _) s = do
    newNet <- createRandomNetwork arch
    return $ s { net = newNet, history = [] }
handle _ s = return s

step dt s = do
    let next = iterate (trainDataset lr dat) (net s) !! 10
        err = sum [ (head (last (forward i next)) - head t) ** 2 | (i, t) <- dat ] / 4
    return $ s { net = next, history = realToFrac err : h, t = t s + dt }
  where h = take 600 (history s)

main :: IO ()
main = do
    putStrLn "Starting Neural Network Visualizer..."
    initNet <- createRandomNetwork arch
    playIO (InWindow "Neural Network In Haskell" (1100, 800) (10, 10))
           (greyN 0.02) 60 (State initNet [] 0) (return . render) handle step