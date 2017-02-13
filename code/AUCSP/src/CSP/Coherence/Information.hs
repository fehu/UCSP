-----------------------------------------------------------------------------
--
-- Module      :  CSP.Coherence.Information
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module CSP.Coherence.Information(

  Information, collectInformation
, InformationPiece(..), SomeInformationPiece(..)
, IsInformation(..)

) where

import Data.Maybe (mapMaybe)
import Data.Typeable (Typeable, cast)

import Data.Set (Set)
import qualified Data.Set as Set

-----------------------------------------------------------------------------

type Information = Set SomeInformationPiece

class (Typeable i, Show i, Ord i) => InformationPiece i where
  informationType :: i -> String

data SomeInformationPiece = forall i . InformationPiece i =>
     SomeInformationPiece i


collectInformation :: (InformationPiece i) => (i -> a) -> Information -> [a]
collectInformation f inf = mapMaybe tryCollect $ Set.elems inf
  where tryCollect (SomeInformationPiece i) = f <$> cast i

-----------------------------------------------------------------------------

instance Eq SomeInformationPiece where
  i1 == i2 = compareSomeInformationPiece i1 i2 == EQ

instance Ord SomeInformationPiece where compare = compareSomeInformationPiece


compareSomeInformationPiece (SomeInformationPiece i1) (SomeInformationPiece i2) =
  case informationType i1 `compare` informationType i2
    of EQ  -> maybe failed
                    (compare i1)
                    (cast i2)
       ord -> ord
    where failed = error $ "failed to cast information type in \""
                        ++ show i1 ++ " == " ++ show i2  ++ "\""

-----------------------------------------------------------------------------

class IsInformation a where asInformation :: a -> Information
