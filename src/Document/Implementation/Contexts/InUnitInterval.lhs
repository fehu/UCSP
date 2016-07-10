
%if False
\begin{code}

module Document.Implementation.Contexts.InUnitInterval(

  InUnitInterval, inUnitInterval, fromUnitInterval

) where

import Data.Function (on)

\end{code}
%endif

Contexts make use of some numeric containers that ensure certain
properties for the underlying numbers.


\begin{itemize}
  \item A real number within $[0,1]$ interval.
\begin{code}

newtype InUnitInterval a = InUnitInterval a

inUnitInterval :: (Fractional a, Ord a) => a -> Maybe (InUnitInterval a)
inUnitInterval x  |   x >= 0
                  &&  x <= 1  = Just $ InUnitInterval x
inUnitInterval _  = Nothing

fromUnitInterval (InUnitInterval x) = x


instance (Eq a) => Eq (InUnitInterval a) where
    (==) = (==) `on` fromUnitInterval

instance (Ord a) => Ord (InUnitInterval a) where
    compare = compare `on` fromUnitInterval

instance (Show a) => Show (InUnitInterval a) where
    show = show . fromUnitInterval

\end{code}
\end{itemize}



