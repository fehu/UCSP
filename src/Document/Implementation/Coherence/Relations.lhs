
%if False
\begin{code}

module Document.Implementation.Coherence.Relations(

) where

\end{code}
%endif

\subsubsection{Relations}

\begin{code}

class InformationRelation r where
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


coerceIRelation :: (Coercible a b) => IRelation a -> IRelation b
coerceIRelation (RelBin r)    = RelBin (coerceRelation r)
coerceIRelation (RelWhole r)  = RelWhole (coerceRelation r)

-- -----------------------------------------------

type RelValue a = Either [RelValBetween a] (RelValWhole a)

\end{code}

