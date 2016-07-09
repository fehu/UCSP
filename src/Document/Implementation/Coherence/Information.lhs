
%if False

module Document.Implementation.Coherence.Information (

)
where

%endif


\subsubsection{Information}

The proposed system makes use of the following information:
\begin{enumerate}

 \item \textbf{Personal knowledge}, known only by one actor.
  \begin{enumerate}
    \item \textbf{Capabilites}: information about what an agent can do,
          what kind of arrangments it can make.
    \item \textbf{Obligations}: information about \emph{strong restrictions},
          imposed over the agent.
    \item \textbf{Preferences}: information about \emph{weak restrictions}.
  \end{enumerate}

 \item \textbf{Shared knowledge}, obtained in the negotiation.
  \begin{enumerate}
    \item \textbf{Others' capabilities} --- information about the counterpart
          agents, that are known to be (un-) capable of doing something.
    \item \textbf{Classes proposals}:
          \begin{enumerate}
            \item
              \begin{itemize}[leftmargin=2cm]
                \item[\textbf{Abstract}] --- has no specific time assigned.
                \item[\textbf{Concrete}] --- has a specific time defined.
              \end{itemize}
            \item
              \begin{itemize}[leftmargin=2cm]
                \item[\textbf{Complete}] --- references all three representing
                  agents: a \emph{group}, a \emph{professor} and a
                  \emph{classroom}.
                \item[\textbf{Partial}] --- references less then three representing
                  agents.

              \end{itemize}

          \end{enumerate}
    \item \textbf{Classes decisions}:
          \begin{enumerate}
            \item \textbf{Class acceptance} --- a mark for
                  \emph{accepted classes proposals}. Only \emph{complete}
                  proposals can be accepted; all the three mentioned agents
                  must accept it, or none.
            \item \textbf{Class rejection} --- a mark for
                  \emph{ignored classes proposals}, a result of \emph{yield}
                  decision, discussed in Section~\ref{subsec:yield}.
          \end{enumerate}
  \end{enumerate}

\end{enumerate}

\begin{code}

data InformationScope = Personal | Shared

-- ``Ord'' instance is mainly needed to create ``Set''s.
class (Typeable i, Eq i, Ord i) => InformationPiece i
    where type IScope i :: InformationScope


class (InformationPiece i, Personal ~ IScope i)  => PersonalInformation i
class (InformationPiece i, Shared ~ IScope i)    => SharedInformation i
    where sharedBetween :: i -> Set AgentRef

-- -----------------------------------------------

instance Eq   SomeClass where
    (SomeClass a) == (SomeClass b) = cast a == Just b

                                                                            -- TODO
instance Ord  SomeClass
instance InformationPiece SomeClass where
    type IScope SomeClass = Shared

instance SharedInformation SomeClass

-- -----------------------------------------------

instance Eq   Class
instance Ord  Class
instance InformationPiece Class where type IScope Class = Shared
instance SharedInformation Class


-- -----------------------------------------------

data Information = forall i . InformationPiece i => Information i

collectInf :: (Typeable a) => Information -> Maybe a
collectInf (Information i) = cast i

instance Eq Information where
  (Information i1) == (Information i2) =
    case cast i1 of  Just x  -> x == i2
                     _       -> False

instance Ord Information where
  (Information i1) `compare` (Information i2) = undefined

-- -----------------------------------------------

newtype Needs = Needs(Set Discipline)
    deriving ( Eq, Ord, Show, Typeable )

newtype CanTeach = CanTeach  (Set Discipline)
    deriving ( Eq, Ord, Show, Typeable )

instance InformationPiece Needs
instance InformationPiece CanTeach


\end{code}

