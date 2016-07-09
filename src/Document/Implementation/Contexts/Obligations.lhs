

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
  obligationsRels  :: [IRelation (ZeroOrOne a)]
  }

instance (Num a) => Context Obligations a where
  contextName _ = "Obligations"
  contextInformation  = return . fromNodes . obligationsInfo
  contextRelations    =  return . map coerceIRelation . obligationsRels
  contextThreshold _  = return 0

  combineBinRels    = combineBinRelsStrict
  combineWholeRels  = combineWholeRelsStrict
  combineRels       = combineRelsStrict

-- This constructor should be hidden.
newtype ZeroOrOne a = ZeroOrOne a
complies  = ZeroOrOne 0
fails     = ZeroOrOne 1

\end{code}

