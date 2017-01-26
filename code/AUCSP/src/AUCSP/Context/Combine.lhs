
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

import Control.Arrow

\end{code}
%endif

Some relations combining functions.

\begin{itemize}
  \item Relation values products/muliplication:
\begin{code}

binRelsProduct  :: (Num a)
                => ([(IRelation a, [RelValBetween a])] -> AssessmentDetails c a)
                -> c a -> RelValsBetween a -> Maybe (CBin c a)
binRelsProduct _ _ bRels | null bRels = Nothing
binRelsProduct fdetails _ bRels  = Just $ CBin (val, details)
    where  val  =  product . concatMap (map relValBetween)
                $  Map.elems bRels
           details = fdetails $ Map.assocs bRels


wholeRelsProduct :: (Num a)
                 => ([(IRelation a, RelValWhole a)] -> AssessmentDetails c a)
                 -> c a -> RelValsWhole a -> Maybe (CWhole c a)
wholeRelsProduct _ _ wRels  | null wRels = Nothing
wholeRelsProduct fdetails _ wRels  = Just $ CWhole (val, details)
    where  val = product . map (fst . unwrapRelValWhole) $ Map.elems wRels
           details = fdetails $ Map.assocs wRels

relsProduct combineDetails _ (CBin (b, bd)) (CWhole (w, wd)) = (b * w, bd `combineDetails` wd)

\end{code}
  \item A product of relations mean values:
    $$ \prod\limits_{\mathrm{rel}_2} \mathrm{E}[V_{\mathrm{rel}_2}] $$
\begin{code}

binRelsMeansProduct :: (Fractional a)  => ([(IRelation a, [RelValBetween a], Maybe a)] -> AssessmentDetails c a)
                                       -> c a
                                       -> RelValsBetween a
                                       -> Maybe (CBin c a)
binRelsMeansProduct fdetails _ rels =
    let  mean =  maybeMean . map relValBetween
         (valsBetween, means) = unzip $ (id &&& mean) <$> Map.elems rels
         details = fdetails $ zip3 (Map.keys rels) valsBetween means
    in case catMaybes means of
            []  -> Nothing
            ms  -> Just $ CBin (product ms, details)

maybeMean [] = Nothing
maybeMean xs = Just $ sum xs / fromIntegral (length xs)

\end{code}
\end{itemize}

