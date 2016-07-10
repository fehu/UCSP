
%if False
\begin{code}

module AUCSP.Context.Obligations(

  Obligations(..)

) where

import AUCSP.Coherence
import AUCSP.Context

import qualified AUCSP.Context.Combine as Combine

\end{code}
%endif

\subsubsection{Obligations}
Obligations determine the rest \emph{strong restrictions} over the classes.
Possible obligations might depend on agent's role and are usually determined by
the institution. For example: maximum classes per day, lunch recess,
lower/upper class time limit, two classes must/cannot follow etc.

The expected values are
\begin{itemize}
  \item[0] if the obligation is broken;
  \item[1] otherwise.
\end{itemize}

All the obligations must comply over a candidate.

\begin{code}

data Obligations a  = Obligations  {
  obligationsInfo  :: [Information],
  obligationsRels  :: [IRelation Bool]
  }

obligationToNum True   = 1
obligationToNum False  = 0

instance (Num a) => Context Obligations a where
  contextName _ = "Obligations"
  contextInformation  = return . fromNodes . obligationsInfo
  contextRelations    = return . map (fmap obligationToNum) . obligationsRels
  contextThreshold _  = return 0

  combineBinRels    = Combine.binRelsProduct
  combineWholeRels  = Combine.wholeRelsProduct
  combineRels       = Combine.relsProduct

\end{code}

