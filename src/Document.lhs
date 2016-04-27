\documentclass{article}

%format family = "\textbf{family}"

 % format :: = "\textbf{::}"

%include polycode.fmt
%include forall.fmt

\usepackage{subcaption, hyperref, tikz, ifthen}
\usepackage[english]{babel}
\usepackage[inline, shortlabels]{enumitem}

\usepackage{showframe}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if False
\begin{code}

module Document where

import Data.Ix
import Data.Typeable
import Data.Either

\end{code}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}


\emph{\textbf{\Large Abstract}}

\medskip
\noindent

This article proposes a system for generating possible
\emph{University Classes Schedules}.
It uses multi-agent negotiation to find satisfactory solutions
to the problem, while trying to consider \emph{personal preferences}
of the represented people and institutions.


\section{Implementation}

\subsection{University Classes}

A class is an en event, that brings together a \emph{group of students},
and a \emph{professor} in certain \emph{classroom} in order to
learn/teach the specified \emph{discipline}.
It happens \underline{periodically}, usually weekly,
at the established \emph{day of week} and \emph{time}.

%if False
\begin{code}

data GroupRef      = GroupRef String
data ProfessorRef  = ProfessorRef String
data ClassroomRef  = ClassroomRef String

data Discipline  = DisciplineClass  { disciplineId :: String
                                    , disciplineMinutesPerWeek :: Int
                                    }
                 | DisciplineLab    { disciplineId :: String
                                    , disciplineMinutesPerWeek :: Int
                                    }
\end{code}
%endif

\begin{code}

data Class time = Class  { classDay        :: Day
                         , classBegins     :: time
                         , classEnds       :: time
                         , classDiscipline :: Discipline
                         , classGroup      :: GroupRef
                         , classProfessor  :: ProfessorRef
                         , classRoom       :: ClassroomRef
                         }

-- redefined 'System.Time.Day' -- no 'Sunday'
data Day  =  Monday | Tuesday | Wednesday
          | Thursday | Friday | Saturday
  deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

\end{code}


The classes are negotiated by the interested parties:
\begin{enumerate*}[1)]
  \item students / groups,
  \item professors,
  \item classrooms.
\end{enumerate*}
Each negotiation participant has a \emph{timetable}, holding a
schedule for one week, that repeats throughout the academic period.
The \emph{timetable} is actually a table:
the columns represent days of week; the rows -- discrete time intervals.
Actual timetable structure may vary, as can be seen in figure
\ref{fig:timetables}.

\begin{figure}[h]
  \centering

  \begin{subfigure}{\textwidth}
    \centering
    \begin{tabular}{||c||c||c||c||c||c||c||}
      \hline
      ~ & Mon & Tue & Wed & Thu & Fri & Sat \\ \hline
      08:30 -- 09:00 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      09:00 -- 09:30 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      09:30 -- 10:00 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      10:00 -- 10:30 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      10:30 -- 11:00 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      11:00 -- 11:30 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      11:30 -- 12:00 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      \vdots \qquad\quad \vdots & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
    \end{tabular}

    \caption{Timetable without recesses.}
  \end{subfigure}

  \begin{subfigure}{\textwidth}
    \centering
    \begin{tabular}{||c||c||c||c||c||c||c||}
      \hline
      ~ & Mon & Tue & Wed & Thu & Fri & Sat \\ \hline
      08:30 -- 09:10 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      09:15 -- 09:55 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      10:05 -- 10:45 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      10:50 -- 11:30 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      11:40 -- 12:20 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      12:25 -- 13:05 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      13:15 -- 13:55 & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
      \vdots \qquad\quad \vdots & ~ & ~ & ~ & ~ & ~ & ~ \\ \hline
    \end{tabular}

    \caption{Timetable with recesses.}
  \end{subfigure}

  \caption{Possible \emph{timetable} structures. }
  \label{fig:timetables}
\end{figure}


\begin{code}

class (Ord t, Bounded t, Show t) => DiscreteTime t where
  toMinutes    :: t -> Int
  fromMinutes  :: Int -> t

class (DiscreteTime time) => Timetable tt e time  |  tt  -> time
                                                  ,  tt  -> e
                                                  ,  e   -> time
  where  listEvents  :: tt -> [e]
         eventsOn    :: tt -> Day   -> [e]
         eventsAt    :: tt -> time  -> [(Day, e)]
         eventAt     :: tt -> Day   -> time -> Maybe e
\end{code}

One should distinguish the resulting timetables, shown in figure
\ref{fig:timetables} and the timetable, held an agent
during the negotiation. The first one is immutable and is the
result of agent's participation in the negotiation.
The set of such timetables, produced by every the participant,
is the \textbf{university schedule} for given academic period.

During the negotiation, an agent's inner timetable gets changed
on the fly, in order to record agreements made.
This means that we are dealing with \emph{side effects}, that
need to be explicitly denoted in Haskell. The following
definition leaves it free to choose the monad abstraction for
those effects.


\begin{code}
class (DiscreteTime time, Monad m) =>
      TimetableM tt m e time  |  tt  -> time
                              ,  tt  -> e
                              ,  e   -> time
  where  putEvent    :: tt -> e -> m tt
         delEvent    :: tt -> e -> m tt
         ttSnapshot  :: (Timetable ts x time) => tt -> m ts



\end{code}


\subsection{Negotiating Agents}
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
  \item\label{itm:goal} pursue the \emph{common goal} -- it must consider
                                    the \underline{common benefits}, while
                                    being egoistic enough to achieve it's own goal;
  \item respond to the messages received in correspondence with \ref{itm:goal};
  \item initiate conversations (send messages, that are not responses),
    driven by \ref{itm:goal};
  \item become more susceptible (less egoistic) with passage of time.
\end{enumerate}

\subsubsection{Common Goal}
Agent's own \emph{goal} represents its egoistical interests.
They may (and will) contradict another agent's interests, thus
creating \emph{incoherence}. The general rule is this case is
to strive for solutions, benefiting the whole schedule.
Because the schedule doesn't yet exist as a whole during the negotiation,
an agent should consider instead the benefits, obtained by itself and
the rest of the agents.

The \emph{common goal} is incorporated in the \emph{contexts}
mechanism, and is discussed in section \ref{subsec:context-external}.

\subsubsection{Messaging}
 \textbf{Is this section really needed?}

\subsection{Coherence}

The coherence mechanism is based on \cite{Sindhu2010}.
It uses the \emph{contexts} as means of separating (and further prioritizing)
different \emph{cognitive aspects}. The contexts used are based on \emph{BDI}
agent architecture.

The \emph{combined coherence} is used as the a measure of goal achievement.
It's combined of coherence values, calculated by agent's contexts.

\subsubsection{Information and Relations}
The coherence is calculated over an \emph{information graph}, that represents
some aspect of agent's knowledge. The nodes of the graph are some
\emph{pieces of information} and the edges represent some \emph{relations}
between theese pieces.

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
    \item \textbf{Others' capabilities} -- information about the counterpart
          agents, that are known to be (un-)capable of doing something.
    \item \textbf{Classes proposals}:
          \begin{enumerate}
            \item \textbf{Complete} -- references all three representing
                  agents: a \emph{group}, a \emph{professor} and a
                  \emph{classroom}.
            \item \textbf{Partial} -- references less then three representing
                  agents.
          \end{enumerate}
    \item \textbf{Classes decisions}:
          \begin{enumerate}
            \item \textbf{Class acceptance} -- a mark for
                  \emph{accepted classes proposals}. Only \emph{complete}
                  proposals can be accepted; all the three mentioned agents
                  must accept it, or none.
            \item \textbf{Class rejection} -- a mark for
                  \emph{ignored classes proposals}, a result of \emph{yield}
                  decision, discussed in section \ref{subsec:yield}.
          \end{enumerate}
  \end{enumerate}

\end{enumerate}

\medskip\noindent
The \emph{binary relations} connect some information pieces, assigning to
the edge some value. The \emph{whole graph relations}, on the other side,
are applied to the graph as a whole and produce a single value.

The relations used, as well as the information in the graph,
depend on the \emph{context}.

\begin{code}

class (Typeable i) => InformationPiece i

data Information = forall i . InformationPiece i => Information i

-- -----------------------------------------------

data RelValBetween a = RelValBetween {
     relBetween     :: (Information, Information)
  ,  relValBetween  :: a
  }

type RelValsBetween a = [RelValBetween a]

newtype RelValWhole a = RelValWhole a
unwrapRelValWhole (RelValWhole a) = a

-- -----------------------------------------------

class BinaryRelation r where
  binRelValue :: r a -> Information -> Information -> Maybe a

class WholeRelation r where wholeRelValue :: r a -> IGraph -> a


data IRelation a  =  forall r .  BinaryRelation r =>  RelBin (r a)
                  |  forall r .  WholeRelation  r =>  RelWhole (r a)


type RelValue a = Either (RelValsBetween a) (RelValWhole a)

-- -----------------------------------------------

class InformationGraph g where
  graphNodes  :: g -> [Information]
  graphJoin   :: g -> [Information] -> g
  relationOn  :: IRelation a -> g -> RelValue a

data IGraph = forall g . InformationGraph g => IGraph g

instance InformationGraph IGraph where
    graphNodes    (IGraph g)  = graphNodes g
    graphJoin     (IGraph g)  = IGraph . graphJoin g
    relationOn r  (IGraph g)  = r `relationOn` g

\end{code}

\subsubsection{Contexts}
In order to use contexts for information \emph{coherence assessment},
the concepts of \emph{context-specific information graph} and
\emph{assessed information} are introduced.
The context-specific graph holds the information, already known/accepted by the
agent, and is relevant for the context in question.
The assessed one is \emph{assumed} during the evaluation process.

\begin{centering}
\input{ContextAssess.tikz}
\end{centering}
 
To assess some information, it's propagated through the contexts, in the
\emph{specified order}, that stands for contexts priority. Each context
xshould have a \emph{coherence threshold} specified; after the assessed
information's coherence has been estimated, it's compared against the
threshold and either \texttt{Success} or \texttt{Failure} is returned,
along with the evaluated coherence value.
The information, that has successfully passed a context, is propagated
further; otherwise the failure is returned.

\begin{code}

class Context c a | c -> a where
  contextName         :: c -> String
  contextInformation  :: c -> IGraph
  contextRelations    :: c -> [IRelation a]
  contextThreshold    :: c -> IO a

  combineBinRels      :: c -> RelValsBetween a  -> Maybe (BinRelsCombined a)
  combineWholeRels    :: c -> [RelValWhole a]   -> WholeRelsCombined a
  combineRels         :: c -> BinRelsCombined a -> WholeRelsCombined a -> a



newtype BinRelsCombined a    = BinRelsCombined a
newtype WholeRelsCombined a  = WholeRelsCombined a


data AssessmentDetails a -- TODO

data SomeContext a = forall c . Context c a => SomeContext c

-- -----------------------------------------------

assessWithin' ::  (Context c a) =>
                  [Information] -> c -> (Maybe a, AssessmentDetails a)

assessWithin' inf c = (assessed, undefined) -- TODO
  where  assumed = contextInformation c `graphJoin` inf
         (bins, whole)  = partitionEithers
                        $ (`relationOn` assumed) <$> contextRelations c

         rBinMb  = c `combineBinRels`  concat bins
         rWhole  = c `combineWholeRels` whole

         assessed = flip (combineRels c) rWhole <$> rBinMb

-- -----------------------------------------------

data AssessedCandidate a = AssessedCandidate {
       assessedAt       :: SomeContext a
    ,  assessedVal      :: Maybe a
    ,  assessedDelails  :: AssessmentDetails a
    }

data Candidate a   =  Success  {  assessHistory  :: [AssessedCandidate a]
                               ,  candidate      :: [Information]
                               }
                   |  Failure  {  assessHistory  :: [AssessedCandidate a]
                               ,  candidate      :: [Information]
                               }

-- -----------------------------------------------

assessWithin ::  (Context c a, Ord a) =>
                 Candidate a -> c -> IO (Candidate a)

assessWithin f@Failure{} _ = return f
assessWithin (Success hist c) cxt = do
  let  (mbA, details) = c `assessWithin'` cxt
       ac = AssessedCandidate (SomeContext cxt) mbA details
  threshold <- contextThreshold cxt
  return $  if mbA > Just threshold
            then  Success  (ac : hist) c
            else  Failure  (ac : hist) c


\end{code}


\subsubsection{Capabilities}
\subsubsection{Beliefs}
\subsubsection{Obligations}
\subsubsection{Preferences}
\subsubsection{External}
 \label{subsec:context-external}

\subsubsection{Decision}


\subsection{Agent}
 Here follows \emph{agents} implementation.

\end{document}

%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% eval: (haskell-indentation-mode)
%%% eval: (when (not (haskell-session-maybe)) (haskell-session-change))
%%% eval: (load (concat (file-name-as-directory (projectile-project-root)) "publish-pdf.el"))
%%% End:
