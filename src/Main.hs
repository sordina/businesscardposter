

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import Data.List
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Text.InterpolatedString.Perl6

-- Known Dimensions

pwidth, pheight, hrowhspace :: Double
rwidth        = 1017
rheight       = 813
pwidth        = 1000
pheight       = 750
hwidth        = 90
hheight       = 55
vwidth        = hheight
vheight       = hwidth
cardsPerHRow  = 10
cardsPerVRow  = 15
vrows         = 2
hrows         = 8
hborder       = (rwidth  - pwidth ) / 2
vborder       = (rheight - pheight) / 2

description = [qc|
  rwidth        = { rwidth       }
  rheight       = { rheight      }
  pwidth        = { pwidth       }
  pheight       = { pheight      }
  hwidth        = { hwidth       }
  hheight       = { hheight      }
  cardsPerHRow  = { cardsPerHRow }
  cardsPerVRow  = { cardsPerVRow }
  vrows         = { vrows        }
  hrows         = { hrows        }
  hborder       = { hborder      }
  vborder       = { vborder      }
|]

-- Calculated Dimensions

hrowhspace    = (pwidth  - (cardsPerHRow * hwidth )) / spacesPerHRow
vrowhspace    = (pwidth  - (cardsPerVRow * hheight)) / spacesPerVRow
colvspace     = (pheight - (hrows        * hheight) - (vrows * vheight)) / spacesPerCol
spacesPerHRow = succ cardsPerHRow
spacesPerVRow = succ cardsPerVRow
spacesPerCol  = succ (hrows + vrows)
vSpaces       = succ (vrows + hrows)
vspace        = (pheight - (vrows * hwidth + hrows * hheight)) / vSpaces
hrowmidoffset = hrowhspace + hwidth / 2
vrowmidoffset = vrowhspace + vwidth / 2
hcolmidoffset = colvspace  + hheight / 2
vcolmidoffset = colvspace  + vheight / 2

-- Go

main :: IO ()
main = mainWith $ pad 1.1 $ center $ (textLines description # scale 30 # pad 1.1)
   ||| strutX 250
   ||| (hmeasures === ((cards <> canvas) ||| smeasures) === vmeasures)

-- Cards and Canvas and Measurements

hmeasures = left <> ticks
  where
  left          = square 1 # scaleX hrowmidoffset # translateX (hrowmidoffset / 2) # translateY 25
  ticks         = mconcat $ map hmeasure hmidoffsets
  hmeasure    x = square 1 # scaleY 50 # translateX x # translateY 25 ||| mt2 x
  measureText t = (topLeftText t # fontSize (local 20) # translateY 115)
  mt2         x = measureText (take 7 $ show x) === strutY 33 === measureText (take 7 $ show (x + hborder))

vmeasures = left <> ticks
  where
  left          = square 1 # scaleX vrowmidoffset # translateX (vrowmidoffset / 2) # translateY 25
  ticks         = mconcat $ map hmeasure vmidoffsets
  hmeasure    x = square 1 # scaleY 50 # translateX x # translateY 25 ||| mt2 x
  measureText t = topLeftText t # fontSize (local 15) # translateY (-15)
  mt2         x = measureText (take 7 $ show x) === strutY 20 === measureText (take 7 $ show (x + hborder))

smeasures = top <> ticks
  where
  top           = square 1 # scaleY hcolmidoffset # translateY (negate $ hcolmidoffset / 2) # translateX 25
  ticks         = mconcat $ map smeasure smidoffsets
  smeasure    y = square 1 # scaleX 50 # translateY (negate y) # translateX 25 === mt2 y
  measureText t = topLeftText t # fontSize (local 20) # translateX 70
  mt2         y = measureText (take 7 $ show y) === strutY 28 === measureText (take 7 $ show (y + vborder))

hmidoffsets = map zzz indexes
  where
  m       = hwidth + hrowhspace
  zzz x   = m * x + hrowmidoffset
  indexes = zipWith const [0..] (replicate cardsPerHRow ())

vmidoffsets = map zzz indexes
  where
  m       = vwidth + vrowhspace
  zzz x   = m * x + vrowmidoffset
  indexes = zipWith const [0..] (replicate cardsPerVRow ())

smidoffsets = [hcolmidoffset, colvspace + hheight + vcolmidoffset]
           ++ midoffsets
           ++ [lastmid + vcolmidoffset, lastmid + colvspace + vheight + hcolmidoffset]
  where
  m       = hheight + colvspace
  c       = 2 * colvspace + hheight + vheight + hcolmidoffset
  zzz x   = m * x + c
  midindexes = zipWith const [0..] (replicate (hrows - 2) ())
  midoffsets = map zzz midindexes
  lastmid    = (head $ reverse $ midoffsets) + hheight / 2

canvas :: Diagram B
canvas  = square 1 # scaleX pwidth # scaleY pheight # alignTL # fc black # lw none

cards   :: Diagram B
cards    = alignTL $ vcat $ extersperse (strutY vspace) $ topCards ++ midCards ++ botCards
topCards = [ hrow, vrow ]
midCards = replicate 6 $ hrow
botCards = [ vrow, hrow ]
hrow     = alignTL $ hRep spacesPerHRow hcard (strutX hrowhspace)
vrow     = alignTL $ hRep spacesPerVRow vcard (strutX vrowhspace)
hcard    = square 1 # scaleX 90 # scaleY 55 # fcA tyellow # lw none # showOrigin
vcard    = square 1 # scaleY 90 # scaleX 55 # fcA tblue   # lw none # showOrigin

-- Combinators

tyellow = yellow `withOpacity` 0.8
tred    = red    `withOpacity` 0.8
tblue   = blue   `withOpacity` 0.8

hjoin spacer items      = hcat $ intercalate [spacer] (map return items)
hRep  n      space item = hjoin space $ replicate n item
vjoin spacer items      = vcat $ intercalate [spacer] (map return items)

extersperse :: x -> [x] -> [x]
extersperse x xs = [x] ++ intersperse x xs ++ [x]

textLines t = vcat $ map (text' 1) (lines t)

text' s t = topLeftText t # fontSize (local s) <> strutY (s * 1.3)
