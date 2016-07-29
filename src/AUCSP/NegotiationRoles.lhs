
%if False
\begin{code}
module AUCSP.NegotiationRoles(

  RoleIx(..), AnyRole(..), roleIx'

, GroupRole(..)
, ClassroomRole(..)
, ProfessorRole(..)

) where

import Data.Typeable
import Data.Function (on)


\end{code}

%endif


As it was mentioned before, the schedule is formed in
a negotiation between \emph{professors}, \emph{groups}
and \emph{classrooms}. To distinguish those three types
of participants, agent's \underline{role} is introduced.
The role:
\begin{enumerate*}[1)]
  \item identifies the kind of person/entity, represented
    by the agent;
  \item defines agent's reaction on the messages received;
  \item defines agent's \underline{goal}.
\end{enumerate*}

A \emph{representing agent} is a computational entity, that
represents a \emph{real person or object} in it's virtual environment.
In current case, it represents one's interests in a \emph{negotiation}.
Such an agent must
\begin{enumerate}[(1)]
  \item\label{itm:goal} pursue the \emph{common goal} --- it must consider
                                    the \underline{common benefits}, while
                                    being egoistic enough to achieve it's own goal;
  \item respond to the messages received in correspondence with~\ref{itm:goal};
  \item initiate conversations (send messages, that are not responses),
    driven by~\ref{itm:goal};
  \item become more susceptible (less egoistic) with passage of time.
\end{enumerate}

\begin{code}

data GroupRole      = GroupRole      deriving (Show, Typeable)
data ClassroomRole  = ClassroomRole  deriving (Show, Typeable)
data ProfessorRole  = FullTimeProfRole | PartTimeProfRole
    deriving (Show, Typeable)


-- -----------------------------------------------

class RoleIx r where roleIx :: r -> Int

-- -----------------------------------------------

data AnyRole = forall r . (RoleIx r, Show r, Typeable r) => AnyRole r

roleIx' (AnyRole r) = roleIx r

instance Show  AnyRole where show (AnyRole r) = show r
instance Eq    AnyRole where (==) = (==) `on` roleIx'
instance Ord   AnyRole where compare = compare `on` roleIx'

\end{code}

\subsubsection{Common Goal}
Agent's own \emph{goal} represents its egoistical interests.
They may (and will) contradict another agent's interests, thus
creating \emph{incoherence}. The general rule in this case is
to strive for solutions, benefiting the whole schedule.
Because the schedule doesn't yet exist as a whole during the negotiation,
an agent should consider instead the benefits, obtained by itself and
the rest of the agents.

The \emph{common goal} is incorporated in the \emph{contexts}
mechanism, and is discussed in Section~\ref{subsec:context-external}.
