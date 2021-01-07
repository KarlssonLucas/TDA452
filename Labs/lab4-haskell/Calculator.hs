-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Expr

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     sc      <- mkInput 10 "0.04"            -- formula scale
     draw    <- mkButton "Draw graph"         -- The draw button
     diff    <- mkButton "Differentiate"

       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input,pure sc]
     getBody window #+ [column [pure canvas,pure formula,pure draw, pure diff]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     diff  $ \ _ -> diffAndDraw input sc canvas 
     on UI.click     draw  $ \ _ -> readAndDraw input sc canvas
     on valueChange' input $ \ _ -> readAndDraw input sc canvas
     on valueChange' sc    $ \ _ -> readAndDraw input sc canvas


readAndDraw :: Element -> Element -> Canvas -> UI ()
readAndDraw input sc canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     sce <- get value sc
     let e = fromJust $ readExpr formula
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText formula (10,canHeight/2) canvas
     path "blue" [p | p <- points e (read sce :: Double) (canWidth, canHeight)] canvas

diffAndDraw :: Element -> Element -> Canvas -> UI ()
diffAndDraw input sc canvas = do
    formula <- get value input
    sce <- get value sc
    let derived = simplify $ differentiate $ fromJust $ readExpr formula
    clearCanvas canvas
    set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
    input # set' value (showExpr derived) 
    UI.fillText (showExpr derived) (10,canHeight/2) canvas
    path "red" [p | p <- points derived (read sce :: Double) (canWidth, canHeight)] canvas

xpoints :: Double -> Double -> [Double]
xpoints max n
  | n <= -max = []
  | otherwise = next : xpoints max next
  where next = n - max / 200

points :: Expr -> Double -> (Int, Int) -> [Point]
points e scale (w, h) = map (\x  -> (fst x / scale + hh, snd x / scale + wh)) [((x) , -1.0 * eval e (x)) | x <- xpoints (wd*scale) (wd*scale)]
    where wh = (fromIntegral (w `div` 2) :: Double)
          wd = (fromIntegral w) :: Double
          hh = (fromIntegral (h `div` 2) :: Double) 

scale :: Int -> Double -> Double
scale num scale = (fromIntegral num) * scale


