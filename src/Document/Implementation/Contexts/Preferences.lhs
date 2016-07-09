

\subsubsection{Preferences}
Preferences determine \emph{weak restrictions}, that are intended to be
set by the represented person (the institution in case of the classroom).

The expected value must be inside $[0,1]$ (unit) interval. They are combined
as follows:
\begin{itemize}[leftmargin=3cm]
 \item[Binary:]
   \begin{flalign*}
     \forall & \text{ binary preference relation } \mathrm{pref_i} \implies & \\
     ~ & P_\mathrm{bin}^i = \dfrac{\sum\limits_{\langle n_1, n_2 \rangle } \mathrm{pref}_i(n_1,n_2)}
                                { || \lbrace \langle n_1, n_2 \rangle \rbrace ||
                                } & \\
     \\
     ~ & P_\mathrm{bin} = \prod\limits_{i} P_\mathrm{bin}^i; \qquad
     P_\mathrm{bin}^i \in [0,1]; \quad  P_\mathrm{bin} \in [0,1]. & \\
   \end{flalign*}
   % \begin{flalign*}
   %   \forall & \text{ pair of nodes } \langle n_1, n_2  \rangle & \\
   %   \forall & \text{ binary preference relation } \mathrm{pref_i} \implies & \\
   %   ~ & P_\mathrm{bin}[n_1, n_2] = \prod\limits_{i} \mathrm{pref}_i(n_1, n_2);
   %   \quad P_\mathrm{bin} \in [0,1] &\\
   %   \\
   %   ~ & P_\mathrm{bin} = \dfrac{\sum\limits_{\langle n_1, n_2 \rangle } P_\mathrm{bin}(n_1,n_2)}
   %                            { || \lbrace \langle n_1, n_2 \rangle \rbrace || }
   %                            & \\
   % \end{flalign*}
 \item[Whole:]
   \begin{flalign*}
     \forall & \text{ whole graph relation } \mathrm{pref}_i & \\
     ~ & P_\mathrm{whole} = \prod\limits_i \mathrm{pref}_i(\mathrm{graph}) & \\
   \end{flalign*}
 \item[Combined:] $ P = P_\mathrm{whole} \times P_\mathrm{bin} $
\end{itemize}

The context should diminish its influence over time to avoid possible
over-restrictions due to conflicting personal interests.

\begin{code}

data Preferences a = Preferences  {
  preferencesInfo       :: [Information],
  preferencesRels       :: [IRelation (InUnitInterval a)],
  preferencesThreshold  :: IORef a
  }

instance (Fractional a) => Context Preferences a where
  contextName _ = "Preferences"
  contextInformation  = return . fromNodes . preferencesInfo
  contextRelations    = return . map coerceIRelation . preferencesRels
  contextThreshold    = readIORef . preferencesThreshold

  combineBinRels _    = fmap CBin . combineBinRelsMeansProd'
  combineWholeRels _  = fmap CWhole . combineWholeRelsProd'
  combineRels         = combineRelsProd


-- -----------------------------------------------

maybeMean [] = Nothing
maybeMean xs = Just $ sum xs / fromIntegral (length xs)

combineBinRelsMeansProd' :: (Fractional a) => RelValsBetween a -> Maybe a
combineBinRelsMeansProd' = foldr f Nothing
  where  mean' = maybeMean . map relValBetween
         f xs acc@(Just _)  = ((*) <$> acc <*> mean' xs) <|> acc
         f xs _             = mean' xs

combineWholeRelsProd' mp  | null mp = Nothing
combineWholeRelsProd' mp  = Just . product . map unwrapRelValWhole
                          $ Map.elems mp

combineRelsProd _ (CBin bin) (CWhole whole) = bin * whole

-- -----------------------------------------------

newtype InUnitInterval a = InUnitInterval a

inUnitInterval :: (Fractional a, Ord a) => a -> Maybe (InUnitInterval a)
inUnitInterval x  |   x >= 0
                  &&  x <= 1  = Just $ InUnitInterval x
inUnitInterval _  = Nothing

fromUnitInterval (InUnitInterval x) = x

instance Eq (InUnitInterval a) where
instance Ord (InUnitInterval a) where

\end{code}

