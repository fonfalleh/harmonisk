module Stoff where

import System.Random --eh
import Sound.MIDI.File

-- TODO Use Map? fromlist $ zip octave notenames 
octave :: [Int]
octave = [0,1,2,3,4,5,6,7,8,9,10,11,12]

-- Note names starting with c
notenames :: [String]
notenames = ["C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B"]

--Intervals
intervals :: [String]
intervals = 
  [ "perfect unison"   -- "prime"
  , "minor second"     -- "half step"
  , "major second"     -- "whole step"
  , "minor third"
  , "major third"
  , "perfect fourth"
  , "augmented fourth" -- Tritone
  , "perfect fifth"
  , "minor sixth"
  , "major sixth"
  , "minor seventh"
  , "major seventh"
  , "perfect octave"]

-- Diatonic major scale (from any note):
diatonic :: [Int]
diatonic = [0,2,4,5,7,9,11] -- (+12)
-- The diatonic scale only contains major or perfect intervals from the base tone.

naturalminor :: [Int]
naturalminor = [0,2,3,5,7,8,10] -- (+12)

harmonicminor :: [Int]
harmonicminor = [0,2,3,5,7,8,11]

melodicminor :: [Int]
melodicminor = [0,2,3,5,7,9,11]

-- Chords
{- Chords are combination of notes played together.
The most common chords are the triads, which are built up by "stacking thirds"-}

-- Major triad
majtriad :: [Int]
majtriad = [0,4,7] -- "+/- 13" inversions etc
-- Base note + major third + minor third
{- Example: C E G. Food for thought: 13 5 8 (E G c), C/E, an equivalent chord? -}

-- Minor triad
mintriad :: [Int]
mintriad = [0,3,7]

fifthcircle :: [Int]
fifthcircle = take 13 [rem x 12 | x <- [0,7..] ]


fifthnames :: [String]
fifthnames = fifthnames' fifthcircle

--TODO better name, modularity. Fold?

fifthnames' :: [Int] -> [String]
fifthnames' []       = []
fifthnames' (x : xs) = (notenames !! x) : (fifthnames' xs)

-- Analysis stuff
-- I ii iii IV V vi vii
data Chord =
  Chord Note Quality
  deriving Show

data Quality =
  Maj | Min | Diminished |
  deriving Show

type Note = Int

transformChord :: Chord -> [Note]
transformChord (Chord root q) = case q of
  Maj -> map (+root) majtriad
  Min -> map (+root) mintriad


-- Idea : Have function that takes chord progression, outputs things
-- TODO more funstuffs 7, Sus2, Sus4, Dim, 9, 11, 13, add, no... etc
