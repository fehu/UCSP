
%if False
\begin{code}

module AUCSP.Context.Combine(

  binRelsProduct, wholeRelsProduct, relsProduct

, binRelsMeansProduct

) where

import AUCSP.Context
import AUCSP.Coherence

import qualified Data.Map as Map

import Data.Maybe (catMaybes)

\end{code}
%endif

Some relations combining functions.

\begin{itemize}
  \item Relation values products/muliplication:
\begin{code}

binRelsProduct _ bRels  | null bRels = Nothing
binRelsProduct _ bRels  = Just  . CBin . product
                                . concatMap (map relValBetween)
                                $ Map.elems bRels

wholeRelsProduct _ wRels  | null wRels = Nothing
wholeRelsProduct _ wRels  = Just  . CWhole . product
                                  . map unwrapRelValWhole
                                  $ Map.elems wRels

relsProduct _ (CBin b) (CWhole w) = b * w

\end{code}
  \item A product of relations mean values:
    $$ \prod\limits_{\mathrm{rel}_2} \mathrm{E}[V_{\mathrm{rel}_2}] $$
\begin{code}

binRelsMeansProduct :: (Fractional a) => x -> RelValsBetween a -> Maybe (CBin a)
binRelsMeansProduct _ rels =
    case catMaybes $ mean <$> Map.elems rels of
            []  -> Nothing
            ms  -> Just . CBin $ product ms
    where mean = maybeMean . map relValBetween

maybeMean [] = Nothing
maybeMean xs = Just $ sum xs / fromIntegral (length xs)

\end{code}
\end{itemize}

