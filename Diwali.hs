module Main
( main
) where

import           Prelude hiding (interact, lines, unlines, words)
import           Data.ByteString.Char8 (pack, ByteString, words, lines, interact, readInt)
import           Data.List (sort, group)
import           Data.Maybe (fromJust)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Array (Array, array, (!))
import qualified Data.Array as A
import           Control.Arrow ((&&&))

type N = Int

type Building = N
type Floor = N
type Loc = (Building, Floor)

type PeopleMap = Map Loc N

diwali :: N -> N -> N -> PeopleMap -> N
diwali builds h loss pm = maxOf ! 1
    where savedAt :: Array Loc N
          savedAt = array ((1, 1), (builds, h))
                    [((b, f), calcSavedAt b f) | b <- [1..builds],
                                                 f <- [1..h]]
          calcSavedAt b f = let above = if f < h then savedAt ! (b, f + 1)
                                                 else 0
                                jump  = if f + loss <= h then maxOf ! (f + loss)
                                                         else 0
                             in max above jump + numPeople pm b f
          maxOf :: Array Floor N
          maxOf = array (1, h) [(f, maxOf' f) | f <- [1..h]]
          maxOf' f = maximum $ map (\b -> savedAt ! (b, f)) [1..builds]
    

numPeople :: PeopleMap -> Building -> Floor -> N
numPeople m b f = M.findWithDefault 0 (b, f) m

handleInput :: ByteString -> ByteString
handleInput = pack . show . coerceInput . map (map toInt . words) . lines
    where toInt :: ByteString -> N
          toInt = fst . fromJust . readInt
          coerceInput ([buildings, height, jumpLoss]
                      : people
                      ) = diwali buildings height jumpLoss
                                 (M.fromList . concat
                                 . zipWith annotate [1..]
                                 . map perBuilding $ people)
          coerceInput _ = error "invalid input"
          annotate :: Building -> [(Floor, N)] -> [(Loc, N)]
          annotate b = map (\(f, n) -> ((b, f), n))
          perBuilding :: [Floor] -> [(Floor, N)]
          perBuilding =  group' . sort . drop 1
          group' = group'' []
          group'' ((a, n):acc) (x:xs) | a == x = let n' = n + 1 in n' `seq` group'' ((a, n') : acc) xs
          group'' acc          (x:xs) = group'' ((x, 1) : acc) xs
          group'' acc          []     = acc


main :: IO ()
main = interact handleInput
