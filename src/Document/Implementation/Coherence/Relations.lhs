
%if False
\begin{code}

module Document.Implementation.Coherence.Relations(

  InformationRelation(..)
, BinaryRelation(..), BinaryIORelation(..)
, WholeRelation(..)

, IRelation(RelBin, RelBinIO, RelWhole)
, relName, coerceIRelation

, RelValBetween(..), RelValsBetween
, RelValWhole(..), RelValsWhole, unwrapRelValWhole

, IGraph, graphNodes, graphJoin, fromNodes, relationOn

, RelValue, IOMaybe

) where

import Document.Implementation.Coherence.Information

import Data.Typeable
import Data.Coerce (Coercible)
import Data.Function (on)
import Data.Maybe

import Data.Set (Set, union, member)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

\end{code}
%endif

\begin{code}


newtype IGraph = IGraph (Set Information)

graphNodes :: IGraph -> [Information]
graphNodes (IGraph inf) = Set.toList inf

graphJoin :: IGraph -> [Information] -> IGraph
graphJoin (IGraph inf) new = IGraph (inf `union` Set.fromList new)

fromNodes :: [Information] -> IGraph
fromNodes = IGraph . Set.fromList

relationOn :: (Num a, Typeable a, Show a) => IRelation a -> IGraph -> IO (RelValue a)
relationOn rel iGraph = case rel of
    RelBin r -> return . Left $ do  i1  <- graphNodes iGraph
                                    i2  <- graphNodes iGraph
                                    if i1 == i2 then []
                                    else maybeToList $
                                         RelValBetween (i1, i2) <$>
                                         binRelValue r i1 i2

    RelBinIO r -> fmap (Left . concat) . sequence $ do
                    i1  <- graphNodes iGraph
                    i2  <- graphNodes iGraph
                    return $ if i1 == i2  then return []
                                          else fmap  ( maybeToList
                                                     . fmap (RelValBetween (i1, i2)))
                                                     (binRelIOValue r i1 i2)

    RelWhole r -> return . Right . RelValWhole $ wholeRelValue r iGraph

\end{code}


\subsubsection{Relations}

\begin{code}

class (Functor r) => InformationRelation r where
  relationName    :: r a -> String
  coerceRelation  :: (Coercible a b) => r a -> r b

class InformationRelation r =>
    BinaryRelation r where
        binRelValue :: (Num a) => r a -> Information -> Information -> Maybe a

class InformationRelation r =>
    WholeRelation r where
        wholeRelValue :: r a -> IGraph -> a

class InformationRelation r =>
    BinaryIORelation r where
        binRelIOValue :: (Num a, Typeable a, Show a) => r a -> Information -> Information -> IOMaybe a

type IOMaybe a = IO (Maybe a)

-- -----------------------------------------------

data RelValBetween a = RelValBetween {
     relBetween     :: (Information, Information)
  ,  relValBetween  :: a
  }

type RelValsBetween a = Map (IRelation a) [RelValBetween a]


newtype RelValWhole a = RelValWhole a
unwrapRelValWhole (RelValWhole a) = a

type RelValsWhole a = Map (IRelation a) (RelValWhole a)

-- -----------------------------------------------

data IRelation a  =  forall r .  BinaryRelation r    =>  RelBin (r a)
                  |  forall r .  BinaryIORelation r  =>  RelBinIO (r a)
                  |  forall r .  WholeRelation  r    =>  RelWhole (r a)

relName (RelBin a)    = relationName a
relName (RelWhole a)  = relationName a

instance Eq (IRelation a) where (==) = (==) `on` relName

instance Ord (IRelation a) where compare = compare `on` relName

mapIRel :: (a -> b) -> IRelation a -> IRelation b
mapIRel f (RelBin r) = RelBin $ fmap f r

instance Functor IRelation where
    fmap f (RelBin r)    = RelBin $ fmap f r
    fmap f (RelBinIO r)  = RelBinIO $ fmap f r
    fmap f (RelWhole r)  = RelWhole $ fmap f r

coerceIRelation :: (Coercible a b) => IRelation a -> IRelation b
coerceIRelation (RelBin r)    = RelBin (coerceRelation r)
coerceIRelation (RelBinIO r)  = RelBinIO (coerceRelation r)
coerceIRelation (RelWhole r)  = RelWhole (coerceRelation r)

-- -----------------------------------------------

type RelValue a = Either [RelValBetween a] (RelValWhole a)

\end{code}


