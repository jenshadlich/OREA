
module Util.Parallel (parallelMap) where

import Control.Parallel (par, pseq)

--------------------------------------------------------------------------------
--parallelMap :: (a -> b) -> [a] -> [b]
--parallelMap _ [] = []
--parallelMap f (x:xs) = y `par` (ys `pseq` y:ys)
--    where
--        y = f x
--        ys = parallelMap f xs

--------------------------------------------------------------------------------
-- | Zur Zeit nur normales map.
parallelMap :: (a -> b) -> [a] -> [b]
parallelMap = map