module Main
( main
) where

import           Prelude hiding (interact, lines, unlines, words, foldr)
import           Data.ByteString.Char8 (pack, ByteString, words, lines, interact, readInt)
import           Data.List (sort, group, foldl')
import           Data.Maybe (fromJust, isJust, fromMaybe, maybe, mapMaybe,
                             catMaybes)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Array.Unboxed (Array, array, (!))
import qualified Data.Array.Unboxed as A
import           Data.Ord (comparing, Down(..))
import           Control.Monad.State (State)
import qualified Control.Monad.State as S
import           Data.Traversable (sequence, sequenceA, traverse,
                                   Traversable)
import           Control.Applicative (Applicative, pure,
                                      (<$>), (<*>), liftA2, liftA3)
import           Data.Foldable (Foldable, foldr)
import           Data.Monoid ((<>), Monoid, mappend, mempty)
import           Control.Arrow ((***), (&&&), first, second)
import           Control.Monad (unless, when, void, filterM, ap)
import           Debug.Trace

type N = Int

type Building = N
type Floor = N

diwali :: N -> N -> N -> [[(Floor, N)]] -> N
diwali builds h loss = qrhead' . fst . foldl' go start . byFloor h
    where start = (qfromList (replicate loss 0), repeat 0)
          go :: (Queue N, [N]) -> [N] -> (Queue N, [N])
          go (q, prev) xs = let (jump, q') = qpopQ' q
                                xs' = zipWith (\a b -> max a jump + b) prev xs
                                q'' = qrpush (maximum xs') q'
                             in (q'', xs')

byFloor :: Floor -> [[(Floor, N)]] -> [[N]]
byFloor 0 _ = []
byFloor n xs = let splt = map (span ((==n) . fst)) xs
                   (fs, xs') = (map fst splt, map snd splt)
                   fs' = map (snd . defHead (n, 0)) fs
                   n' = n - 1
                in n' `seq` fs' : byFloor n' xs'

defHead :: a -> [a] -> a
defHead _ (x:_) = x
defHead d _ = d

handleInput :: ByteString -> ByteString
handleInput = pack . show . coerceInput . map (map toInt . words) . lines
    where toInt :: ByteString -> N
          toInt = fst . fromJust . readInt
          coerceInput ([buildings, height, jumpLoss]
                      : people
                      ) = diwali buildings height jumpLoss
                                 (map perBuilding  people)
          coerceInput _ = error "invalid input"
          perBuilding :: [Floor] -> [(Floor, N)]
          perBuilding =  group' . sort . drop 1
          group' = group'' []
          group'' ((a, n):acc) (x:xs) | a == x = let n' = n + 1 in n' `seq` group'' ((a, n') : acc) xs
          group'' acc          (x:xs) = group'' ((x, 1) : acc) xs
          group'' acc          []     = acc

main :: IO ()
main = interact handleInput

-- Queue ---------------------------------------------------------
-- Note: not to be used in a persistant fashion
-- (amortized O(1) bounds on all simple operations only if you don't
--  back up and use old versions)
data Queue a = Queue Int [a] [a]
    deriving (Eq, Ord)

instance Show a => Show (Queue a) where
    show = ("qfromList " ++) . show . qtoList

qempty :: Queue a
qempty = Queue 0 [] []

qsize :: Queue a -> Int
qsize (Queue n _ _) = n

qfix :: Queue a -> Queue a
qfix (Queue n fo@(_:_:_) []) = let (f, r) = halfRev n fo in Queue n f r
qfix (Queue n [] ro@(_:_:_)) = let (r, f) = halfRev n ro in Queue n f r
qfix q = q

halfRev :: Int -> [a] -> ([a], [a])
halfRev n = second reverse . splitAt (n `div` 2)

qpush :: a -> Queue a -> Queue a
qpush x (Queue n f r) = qfix $ Queue (n+1) (x:f) r

qrpush :: a -> Queue a -> Queue a
qrpush x (Queue n f r) = qfix $ Queue (n+1) f (x:r)

qpopQ :: Queue a -> Maybe (a, Queue a)
qpopQ (Queue n (f:fs) r) = Just (f, qfix $ Queue (n-1) fs r)
qpopQ (Queue _ [] [r]) = Just (r, qempty)
qpopQ _ = Nothing

qpop :: Queue a -> Maybe (Queue a)
qpop = fmap snd . qpopQ

qpop' :: Queue a -> Queue a
qpop' = fromMaybe qempty . qpop

qpopQ' :: Queue a -> (a, Queue a)
qpopQ' = fromJust . qpopQ

qhead :: Queue a -> Maybe a
qhead = fmap fst . qpopQ

qhead' :: Queue a -> a
qhead' = fromJust . qhead

qrpopQ :: Queue a -> Maybe (a, Queue a)
qrpopQ (Queue n f (r:rs)) = Just (r, qfix $ Queue (n-1) f rs)
qrpopQ (Queue _ [f] []) = Just (f, qempty)
qrpopQ _ = Nothing

qrpop :: Queue a -> Maybe (Queue a)
qrpop = fmap snd . qrpopQ

qrpop' :: Queue a -> Queue a
qrpop' = fromMaybe qempty . qrpop

qrpopQ' :: Queue a -> (a, Queue a)
qrpopQ' = fromJust . qrpopQ

qrhead :: Queue a -> Maybe a
qrhead = fmap fst . qrpopQ

qrhead' :: Queue a -> a
qrhead' = fromJust . qrhead

qfromList :: [a] -> Queue a
qfromList xs = qfix $ Queue (length xs) xs []

qtoList :: Queue a -> [a]
qtoList (Queue _ f r) = f ++ reverse r

instance Functor Queue where
    fmap g (Queue n f r) = Queue n (g <$> f) (g <$> r)

instance Foldable Queue where
    foldr g z = foldr g z . qtoList

instance Monad Queue where
    m >>= f = qfromList $ qtoList m >>= (qtoList . f)
    return = qfromList . return

instance Applicative Queue where
    (<*>) = ap
    pure = return

instance Traversable Queue where
    traverse f t = qfromList <$> traverse f (qtoList t)

instance Monoid (Queue a) where
    mappend a b = qfromList $ qtoList a `mappend` qtoList b
    mempty = qempty
-- End of Queue --------------------------------------------------
