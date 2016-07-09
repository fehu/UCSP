
%if False
\begin{code}
 
module Document.Implementation.Coherence(

) where
 

\end{code}
%endif

\subsection{Coherence}

The coherence mechanism is based on~\cite{Sindhu2010}.
It uses the \emph{contexts} as means of separating (and further prioritizing)
different \emph{cognitive aspects}. The contexts used are based on \emph{BDI}
agent architecture.

The \emph{combined coherence} is used as a measure of goal achievement.
It's combined of coherence values, calculated by agent's contexts.

\medskip\noindent
The \emph{binary relations} connect some information pieces, assigning to
the edge some value. The \emph{whole graph relations}, on the other side,
are applied to the graph as a whole and produce a single value.

The relations used, as well as the information in the graph,
depend on the \emph{context}.

The coherence is calculated over an \emph{information graph}, that represents
some aspect of agent's knowledge. The nodes of the graph are some
\emph{pieces of information} and the edges represent some \emph{relations}
between theese pieces.


%include Coherence/Information.lhs
%include Coherence/Relations.lhs

\subsubsection{Information graph}
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


