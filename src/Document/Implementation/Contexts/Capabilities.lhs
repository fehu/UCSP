

\subsubsection{Capabilities}
The capabilities context handles question ``Am I able to do it?''.
It's main purpose is to discard immediately any proposal that
would never be accepted.

\begin{itemize}
  \item \emph{Group}: ``Am I interested in the discipline?''
  \item \emph{Professor}: ``Am I qualified to teach the disciple?''
  \item \emph{Classroom}: ``Do I suit the disciple?'',
                          ``Do I have the capacity required?''
\end{itemize}

\noindent
An agent should mark any other agent, that has declined some proposal for
\emph{capabilities} reasons, describing the reason. It should
further avoid making same kind of proposals to the uncapable agent.

\begin{figure}[h]
  \centering
  \input{Capabilities.tikz.tex}
  \caption{Capabilities required to form a \emph{class}.}
  \label{fig:capabilities}
\end{figure}

\begin{code}

data family Capabilities (r :: NegotiationRole) :: * -> *

data instance Capabilities GroupRole a = GroupCapabilities {
  needsDisciplines :: [Discipline]
  }

data instance Capabilities FullTimeProfRole a = FullTimeProfCapabilities {
  canTeachFullTime :: [Discipline]
  }

-- -----------------------------------------------

data CanTeachRel a = CanTeachRel

instance InformationRelation CanTeachRel where
    relationName _ = "CanTeach"
    coerceRelation = coerce

instance BinaryRelation CanTeachRel where
    binRelValue _ a b =
     let v ds c = if classDiscipline c `member` ds then 1 else 0
     in case collectInf a of
        Just (CanTeach ds)  -> let
            r1  = case collectInf b of Just (SomeClass c)  -> Just $ v ds c
            r2  = case collectInf b of Just (Class c)      -> Just $ v ds c
            in  r1 <|> r2
        _                   -> Nothing

-- -----------------------------------------------

data NeedsDisciplineRel a = NeedsDisciplineRel

instance InformationRelation NeedsDisciplineRel where  -- TODO
instance BinaryRelation NeedsDisciplineRel where       -- TODO

-- -----------------------------------------------

-- product X
combineBinRelsStrict _ bRels  | null bRels = Nothing
combineBinRelsStrict _ bRels  = Just . CBin . product
                              . concatMap (map relValBetween)
                              $ Map.elems bRels

combineWholeRelsStrict _ wRels  | null wRels = Nothing
combineWholeRelsStrict _ wRels  = Just . CWhole . product
                                . map unwrapRelValWhole
                                $ Map.elems wRels

combineRelsStrict _ (CBin b) (CWhole w) = b * w

-- -----------------------------------------------

instance (Num a) => Context (Capabilities GroupRole) a where
  contextName _       = "Capabilities"
  contextInformation  = return . fromNodes . (:[])
                      . Information . Needs
                      . Set.fromList . needsDisciplines
  contextRelations _  = return [RelBin NeedsDisciplineRel]
  contextThreshold _  = return 0

  combineWholeRels    = combineWholeRelsStrict
  combineBinRels      = combineBinRelsStrict
  combineRels         = combineRelsStrict


instance (Num a) => Context (Capabilities FullTimeProfRole) a where
  contextName _       = "Capabilities"
  contextInformation  = return . fromNodes . (:[])
                      . Information . CanTeach
                      . Set.fromList . canTeachFullTime
  contextRelations _  = return [RelBin CanTeachRel]
  contextThreshold _  = return 0

  combineWholeRels    = combineWholeRelsStrict
  combineBinRels      = combineBinRelsStrict
  combineRels         = combineRelsStrict

\end{code}

