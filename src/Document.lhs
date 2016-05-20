\documentclass{article}

%format family = "\textbf{family}"

%format `union` = "\cup"
%format `compare` = "\lesseqqgtr"
%format `member` = "\in"

%format <|> = "\mathrel{\mathord{<}\mathord{|}\mathord{>}}"
%format <$> = "\mathrel{\mathord{<}\mathord{\$}\mathord{>}}"
%format <*> = "\mathrel{\mathord{<}\mathord{*}\mathord{>}}"
%format &&& = "\mathrel{\mathord{\&}\mathord{\&}\mathord{\&}}"


%format i1 = "i_1"
%format i2 = "i_2"
%format AnyFunc1 = "\mathrm{AnyFunc}_1"

%include polycode.fmt
%include forall.fmt

\usepackage{subcaption, hyperref, float, amsmath}
\usepackage{tikz, ifthen, xcolor}
\usepackage[english]{babel}
\usepackage[inline, shortlabels]{enumitem}

\usepackage{showframe}

\usetikzlibrary{fit, calc, arrows, shapes}

\newcommand{\red}[1]{{\color{red} #1}}
\newcommand{\todo}[1]{\red{\{ \textbf{TODO:} #1 \}}}

\newcommand{\crule}[2][1pt]{\begin{center}\rule{#2\textwidth}{#1}\end{center}}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if False
\begin{code}

module Document where

import Data.Ix
import Data.Typeable
import Data.Either
import Data.Function (on)
import Data.IORef
import Data.Maybe

import Data.Set (Set, union, member)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Arrow
import Control.Applicative
import Control.Monad.Fix

import GHC.Exts (groupWith)

\end{code}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\title{ \\[5em]
        UCSP: Implementation
        \\[3em]
      }
\author{Dmitry K.}
\date{ \vfill\today }

\begin{document}


\begin{titlepage}
\maketitle
\thispagestyle{empty}
\end{titlepage}

\begin{abstract}

This article proposes a system for generating possible
\emph{University Classes Schedules}.
It uses multi-agent negotiation to find satisfactory solutions
to the problem, while trying to consider \emph{personal preferences}
of the represented people and institutions.

\end{abstract}
\bigskip
\tableofcontents
\newpage





\section{Implementation}

\subsection{University Classes}

A class is an en event, that brings together a \emph{group of students},
and a \emph{professor} in certain \emph{classroom} in order to
learn/teach the specified \emph{discipline}.
It happens \underline{periodically}, usually weekly,
at the established \emph{day of week} and \emph{time}.

A \emph{discipline} should describe an atomic (not dividable) educational
activity. For example, if the students are required to take a normal class
and also do some specific laboratory practice, then two disciplines should
be created, one of them describing the required lab equipment.


\begin{code}

data Discipline  = Discipline  {  disciplineId              :: String
                               ,  disciplineMinutesPerWeek  :: Int
                               ,  disciplineRequirements    :: Set Requirement
                               }
                 deriving (Typeable, Show, Eq, Ord)


newtype Requirement = Requirement String deriving (Show, Eq, Ord)

\end{code}


\medskip\noindent
For inner usage, the classes are divided into
\begin{itemize}
 \item \emph{abstract} --- without day and time;
 \item \emph{concrete} --- with full time information.
\end{itemize}

\begin{code}

class (Ord c, Show c, Typeable c) =>
    AbstractClass c where  classDiscipline :: c -> Discipline
                           classGroup      :: c -> GroupRef
                           classProfessor  :: c -> ProfessorRef
                           classRoom       :: c -> ClassroomRef
                           classNumber     :: c -> Word

class (AbstractClass c, DiscreteTime time) =>
  ConcreteClass c time | c -> time
    where  classDay     :: c -> Day
           classBegins  :: c -> time
           classEnds    :: c -> time

data Class      = forall c time  . ConcreteClass c time  => Class c
data SomeClass  = forall c       . AbstractClass c       => SomeClass c

\end{code}

The ``System.Time.Day'' is redefined, dropping the ``Sunday''.

\begin{code}
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
the columns represent days of week; the rows --- discrete time intervals.
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

class (Ord t, Bounded t, Show t, Typeable t) => DiscreteTime t where
  toMinutes    :: t -> Int
  fromMinutes  :: Int -> t

data SomeTime = forall t . (DiscreteTime t) => SomeTime t

someTimeMinutes (SomeTime t) = toMinutes t

instance Eq   SomeTime where  (==)     = (==)     `on` someTimeMinutes
instance Ord  SomeTime where  compare  = compare  `on` someTimeMinutes

\end{code}


\begin{code}
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
  \item\label{itm:goal} pursue the \emph{common goal} --- it must consider
                                    the \underline{common benefits}, while
                                    being egoistic enough to achieve it's own goal;
  \item respond to the messages received in correspondence with~\ref{itm:goal};
  \item initiate conversations (send messages, that are not responses),
    driven by~\ref{itm:goal};
  \item become more susceptible (less egoistic) with passage of time.
\end{enumerate}

\begin{code}

data NegotiationRole  = GroupRole
                      | FullTimeProfRole
                      | PartTimeProfRole
                      | ClassroomRole
    deriving (Show, Typeable)

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

\subsubsection{Messaging}
 \textbf{Is this section really needed?}

\subsection{Coherence}

The coherence mechanism is based on~\cite{Sindhu2010}.
It uses the \emph{contexts} as means of separating (and further prioritizing)
different \emph{cognitive aspects}. The contexts used are based on \emph{BDI}
agent architecture.

The \emph{combined coherence} is used as a measure of goal achievement.
It's combined of coherence values, calculated by agent's contexts.

\subsubsection{Information and Relations}
The coherence is calculated over an \emph{information graph}, that represents
some aspect of agent's knowledge. The nodes of the graph are some
\emph{pieces of information} and the edges represent some \emph{relations}
between theese pieces.


\begin{code}


newtype IGraph = IGraph (Set Information)

graphNodes :: IGraph -> [Information]
graphNodes (IGraph inf) = Set.toList inf

graphJoin :: IGraph -> [Information] -> IGraph
graphJoin (IGraph inf) new = IGraph (inf `union` Set.fromList new)

fromNodes :: [Information] -> IGraph
fromNodes = IGraph . Set.fromList

relationOn :: IRelation a -> IGraph -> RelValue a
relationOn rel (IGraph inf) = undefined -- TODO

\end{code}

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

\medskip\noindent
The \emph{binary relations} connect some information pieces, assigning to
the edge some value. The \emph{whole graph relations}, on the other side,
are applied to the graph as a whole and produce a single value.

The relations used, as well as the information in the graph,
depend on the \emph{context}.

\begin{code}

data RelValBetween a = RelValBetween {
     relBetween     :: (Information, Information)
  ,  relValBetween  :: a
  }

type RelValsBetween a = Map (IRelation a) [RelValBetween a]


newtype RelValWhole a = RelValWhole a
unwrapRelValWhole (RelValWhole a) = a

type RelValsWhole a = Map (IRelation a) (RelValWhole a)


-- -----------------------------------------------

class InformationRelation r where relationName :: r a -> String

class InformationRelation r =>
    BinaryRelation r where
        binRelValue :: (Num a) => r a -> Information -> Information -> Maybe a

class InformationRelation r =>
    WholeRelation r where
        wholeRelValue :: r a -> IGraph -> a

-- -----------------------------------------------

data IRelation a  =  forall r .  BinaryRelation r =>  RelBin (r a)
                  |  forall r .  WholeRelation  r =>  RelWhole (r a)

relName (RelBin a)    = relationName a
relName (RelWhole a)  = relationName a

instance Eq (IRelation a) where (==) = (==) `on` relName

instance Ord (IRelation a) where compare = compare `on` relName

type RelValue a = Either [RelValBetween a] (RelValWhole a)


\end{code}

\subsubsection{Contexts}
In order to use contexts for information \emph{coherence assessment},
the concepts of \emph{context-specific information graph} and
\emph{assessed information} are introduced.
The context-specific graph holds the information, already known/accepted by the
agent, and is relevant for the context in question.
The assessed one is \emph{assumed} during the evaluation process.

\begin{figure}[H]
  \centering
  \fbox{ \input{ContextAssess.tikz} }
  \caption{Binary relations within an information graph. One can
           distinguish the relations between the assessed information pieces
           and the relations between assessed and the known ones.
          }
\end{figure}

To assess some information, it's propagated through the contexts, in the
\emph{specified order}, that stands for contexts priority. Each context
should have a \emph{coherence threshold} specified; after the assessed
information's coherence has been estimated, it's compared against the
threshold and either \texttt{Success} or \texttt{Failure} is returned,
along with the evaluated coherence value.
The information, that has successfully passed a context, is propagated
further; otherwise the failure is returned.

\begin{code}

class Context (c :: * -> *) a where
  contextName         :: c a -> String
  contextInformation  :: c a -> IO IGraph
  contextRelations    :: c a -> IO [IRelation a]
  contextThreshold    :: c a -> IO a

  combineBinRels      :: c a -> RelValsBetween a    -> Maybe (CBin a)
  combineWholeRels    :: c a -> RelValsWhole a      -> Maybe (CWhole a)
  combineRels         :: c a -> CBin a -> CWhole a  -> a


newtype CBin a    = CBin a
newtype CWhole a  = CWhole a


data AssessmentDetails a -- TODO

data SomeContext a = forall c . Context c a => SomeContext (c a)

-- -----------------------------------------------

type AnyFunc1 res = forall a . a -> res a

mapEither :: AnyFunc1 r -> Either a b -> Either (r a) (r b)
mapEither f (Left a)   = Left $ f a
mapEither f (Right a)  = Right $ f a

assessWithin' ::  (Context c a) =>
                  [Information]
              ->  c a
              ->  IO (Maybe a, AssessmentDetails a)

assessWithin' inf c = do
  contextInf   <- contextInformation c
  contextRels  <- contextRelations c

  let  assumed = contextInf `graphJoin` inf
       (bins, whole)  = partitionEithers
                      $ (\r -> mapEither ((,) r) $ r `relationOn` assumed)
                      <$> contextRels

       assessed = do  rBin    <- c `combineBinRels`    Map.fromList bins
                      rWhole  <- c `combineWholeRels`  Map.fromList whole
                      return  $ combineRels c rBin rWhole
  return (assessed, undefined)

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
                 Candidate a -> c a -> IO (Candidate a)

assessWithin f@Failure{} _ = return f
assessWithin (Success hist c) cxt = do
  (mbA, details)  <- c `assessWithin'` cxt
  threshold       <- contextThreshold cxt
  let ac = AssessedCandidate (SomeContext cxt) mbA details

  return $  if mbA > Just threshold
            then  Success  (ac : hist) c
            else  Failure  (ac : hist) c


\end{code}


\medskip\noindent
Some contexts might also be capable of \emph{splitting}
information graphs into \emph{valid candidates} --
the sub-graphs, that are \emph{valid} at the context.
The candidates can be assessed by the rest of the contexts.

\begin{code}

class (Context c a) => SplittingContext c a where
  splitGraph :: c a -> IGraph -> IO [Candidate a]


\end{code}

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

  -- Every capability must be coherent. 0*X = 0

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


\subsubsection{Beliefs}
The beliefs is a \emph{splitting} context, that uses as it's internal
knowledge:
\begin{enumerate*}[1)]
 \item \emph{state of the timetable}, that represents
       \emph{best} candidate, generated until now;
 \item \emph{interesting} proposals, both generated by
       agent itself and received from the others,
       that are preserved throughout agent's lifetime.
\end{enumerate*}

\bigskip\noindent
\textbf{Assessing} yields one of three values $$
\begin{cases}
 -1 & \mbox{if two proposals intersect in time} \\
  0 & \mbox{if both proposals have the same \emph{abstract} part} \\
  1 & \mbox{otherwise}
\end{cases} $$

\begin{figure}[h]
  \centering
  \input{BeliefsNewProposal.tikz}
  \caption{Assessing proposal coherence, starting from \emph{Beliefs} context.}
  \label{fig:AssessBeliefs}
\end{figure}

\red{should be written in another place, not in this context}

The assessment of \emph{concrete proposals} (containing concrete classes)
in the graph consists in
\begin{enumerate}
  \item \emph{assuming} the proposal information;
  \item \emph{splitting} the assumed information graph into valid candidates;
  \item \emph{propagating} of the candidates through the rest of the contexts;
  \item comparing the \emph{best candidate} with the previous \emph{best}.
\end{enumerate}

\red{?} The proposal is called \emph{interesting} and is
accepted (and the assumed graph becomes the new information graph
of \emph{beliefs} context)
if it's assumption causes better candidate generation.
It's rejected otherwise (and the assumed graph is discarded).


\bigskip\noindent
\textbf{Splitting} is a process of extraction of \emph{acceptable} sub-graphs,
that compares the coherence values at graph's edges against a threshold.
The splitting can be achieved with one of two following strategies:
\begin{enumerate}
  \item \emph{Joining} proposals while validness is preserved.
  \item \emph{Partitioning} of proposals until validness is achieved.
\end{enumerate}

First strategy is used in this project, due to less memory consumption
(it doesn't have to generate or store big invalid graphs,
that would be present at the first steps of the second strategy).

The splitting is implemented as follows:
\begin{align*}
  \mbox{Let } & C=\lbrace c \rbrace \text{ be a set of \emph{class proposals}}.\\
            ~ & A_i=\lbrace a_i \rbrace \text{ be a set of \emph{acceptable candidates},
                                         composed of } i \text{ proposals.}\\
            ~ & A=\bigcup\limits_{i} A_i \text{ be a set of \emph{acceptable candidates}}.
\end{align*}

\begin{enumerate}
  \item Each single candidate is acceptable:
    $A_1 = \lbrace [ c ] ~||~ \forall ~ c \in C \rbrace$.
  \item Form $A_2$ by extending each candidate $[c'] = a_1 \in A_1$ with $c \in C$,
    if and only if $c'$ and $c$ do not intersect. If $A_1 \not= \emptyset$,
    then try to form $A_2$.
  \item[\vdots]
  \item[i.] Form $A_i$ by extending each candidate $[c'_1, \dots, c'_{i-1}] = a_{i-1}
    \in A_{i-1}$ with $c \in C$, if and only if $\forall c' \in a_{i-1}, ~c'$
    and $c$ do not intersect. If $A_i \not= \emptyset$, then try to form $A_{i+1}$.
   \item[\vdots]
   \item[n.] $A_n = \emptyset \implies$ all the \emph{acceptable candidates}
     were generated. Done.

\end{enumerate}

\crule{0.5}

% 'SomeTime' is removed from shown code to reduce visible expressions length
%{
%format SomeTime (x) = x
\begin{code}
data Beliefs a = Beliefs  {  knownProposals  :: IORef IGraph
--                          ,  bestCandidate   :: IORef (Candidate a, a)
                          }

data TimeConsistency a = TimeConsistency

instance InformationRelation TimeConsistency where
  relationName _ = "TimeConsistency"

instance BinaryRelation TimeConsistency where
  binRelValue _ i1 i2 = do
    Class c1  <- collectInf i1
    Class c2  <- collectInf i2

    let  sameParticipant  =   classGroup c1      ==  classGroup c2
                          ||  classProfessor c1  ==  classProfessor c2
                          ||  classRoom c1       ==  classRoom c2
         sameDay = classDay c1 == classDay c2
         timeIntersects  x y  = SomeTime (classBegins x)  <= SomeTime  (classBegins y)
                             && SomeTime (classEnds x)    >= SomeTime  (classBegins y)

         sameAbstract  =   classDiscipline c1  ==  classDiscipline c2
                       &&  classGroup c1       ==  classGroup c2
                       &&  classProfessor c1   ==  classProfessor c2
                       &&  classRoom c1        ==  classRoom c2
                       &&  classNumber c1      ==  classNumber c2

         intersect  =   sameParticipant
                    &&  sameDay
                    &&  (timeIntersects c1 c2 || timeIntersects c2 c1)

    return $ if sameAbstract  then 0
                              else if intersect then -1 else 1

instance (Num a) => Context Beliefs a where
  contextName _       = "Beliefs"
  contextInformation  = readIORef . knownProposals
  contextRelations _  = return [RelBin TimeConsistency]
  contextThreshold _  = return 0

  combineWholeRels    = combineWholeRelsStrict
  combineBinRels      = combineBinRelsStrict
  combineRels         = combineRelsStrict


instance (Num a) => SplittingContext Beliefs a where
  splitGraph b gr = do
    iGraph <- readIORef $ knownProposals b
    let  cNodes = catMaybes $ collectInf <$> graphNodes gr
         consistent x y = binRelValue TimeConsistency x y == Just 1
         extendCandidate Failure{} = []
         extendCandidate Success{candidate=inf} = do
             c <- cNodes
             [  Success{assessHistory = [], candidate = graphNodes gr ++ [c]}
                | all (consistent c) inf ]

         a1 = Success [] . (:[]) <$> cNodes

    return $ fix (\f acc last ->  let ext = concatMap extendCandidate last
                                    in  if null ext
                                        then acc
                                        else f (acc ++ ext) ext
             ) a1 a1

\end{code}
%}

\subsubsection{Obligations}
Obligations determine the rest \emph{strong restrictions} over the classes.
Possible obligations might depend on agent's role and are usually determined by
the institution. For example: maximum classes per day, lunch recess,
lower/upper class time limit, two classes must/cannot follow etc.

\begin{code}

-- data family Obligations (r :: NegotiationRole) :: * -> *




\end{code}

\subsubsection{Preferences}
Preferences determine \emph{weak restrictions}, that are intended to be
set by the represented person (the institution in case of the classroom).



The context should disminus its influence over time to avoid possible
over-restrictions due to conflicting personal interests.

\begin{code}


\end{code}

\subsubsection{External}
 \label{subsec:context-external}

External contexts take into account the \emph{opinions} of the
agents that are referenced by the solution candidate.
It is responsible for \emph{common goal} assessment.
The assessment must be \emph{objective} --- it must give no preference
to agent's own interests.


\begin{code}


\end{code}

\subsubsection{Decision}


\subsection{Agent}
 Here follows \emph{agents} implementation.

\begin{code}


class AgentComm ag where

class (AgentComm ag) => CommAgentRef ref ag where

    agRef   :: ag -> ref ag
    agComm  :: ref ag -> ag

data AgentRef = forall ref ag . CommAgentRef ref ag => AgentRef (ref ag)

-- -----------------------------------------------

data GroupRef      = GroupRef      String  deriving (Show, Eq, Ord)
data ProfessorRef  = ProfessorRef  String  deriving (Show, Eq, Ord)
data ClassroomRef  = ClassroomRef  String  deriving (Show, Eq, Ord)

\end{code}

\end{document}

 % eval: (when (not (haskell-session-maybe)) (haskell-session-change))

%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% eval: (haskell-indentation-mode)
%%% eval: (interactive-haskell-mode)
%%% eval: (load (concat (file-name-as-directory (projectile-project-root)) "publish-pdf.el"))
%%% End:
