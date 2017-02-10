-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Utils.List
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module AUCSP.Utils.List (

  groupCombinations

) where


-- | Combinates different `c` values, preserving pair lists `[(b,..)]`
-- > groupCombinations [('A', [1,2]), ('B', [3,4])] = [ [('A',1), ('B',3)]
-- >                                                  , [('A',1), ('B',4)]
-- >                                                  , [('A',2), ('B',3)]
-- >                                                  , [('A',2), ('B',4)]
-- >                                                  ]
groupCombinations :: [(a,[b])] -> [[(a,b)]]
groupCombinations ((x,ys) : xys) = do y <- ys
                                      r <- groupCombinations xys
                                      return $ (x,y) : r
groupCombinations [] = [[]]
