

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
pwidth        = 1000
pheight       = 750
hwidth        = 90
hheight       = 55
cardsPerhRow  = 10
cardsPerVRow  = 15
vrows         = 2
hrows         = 8

description = [qc|
  pwidth        = { pwidth       }
  pheight       = { pheight      }
  hwidth        = { hwidth       }
  hheight       = { hheight      }
  cardsPerhRow  = { cardsPerhRow }
  cardsPerVRow  = { cardsPerVRow }
  vrows         = { vrows        }
  hrows         = { hrows        }
|]

-- Calculated Dimensions

hrowhspace    = (pwidth - (cardsPerhRow * hwidth )) / spacesPerHRow
vrowhspace    = (pwidth - (cardsPerVRow * hheight)) / spacesPerVRow
spacesPerHRow = succ cardsPerhRow
spacesPerVRow = succ cardsPerVRow
vSpaces       = succ (vrows + hrows)
vspace        = (pheight - (vrows * hwidth + hrows * hheight)) / vSpaces

-- Go

main :: IO ()
main = mainWith $ pad 1.1 $ center $ (textLines description # scale 30 # pad 1.1) ||| strutX 250 ||| (cards <> canvas)

-- Cards and Canvas

canvas :: Diagram B
canvas  = square 1 # scaleX 1000 # scaleY 750 # alignTL

cards   :: Diagram B
cards    = alignTL $ vcat $ extersperse (strutY vspace) $ topCards ++ midCards ++ botCards
topCards = [ hrow, vrow ]
midCards = replicate 6 $ hrow
botCards = [ vrow, hrow ]
hrow     = alignTL $ hRep spacesPerHRow hcard (strutX hrowhspace)
vrow     = alignTL $ hRep spacesPerVRow vcard (strutX vrowhspace)
hcard    = square 1 # scaleX 90 # scaleY 55
vcard    = square 1 # scaleY 90 # scaleX 55

-- Combinators

hjoin spacer items      = hcat $ intercalate [spacer] (map return items)
hRep  n      space item = hjoin space $ replicate n item
vjoin spacer items      = vcat $ intercalate [spacer] (map return items)

extersperse :: x -> [x] -> [x]
extersperse x xs = [x] ++ intersperse x xs ++ [x]

textLines t = vcat $ map (text' 1) (lines t)

text' s t = topLeftText t # fontSize (local s) <> strutY (s * 1.3)
