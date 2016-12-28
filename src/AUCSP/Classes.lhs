
%if False
\begin{code}

module AUCSP.Classes (

  Discipline(..), Requirement(..)

, AbstractClass(..), ConcreteClass(..)
, Class(..), SomeClass(..)

, Day(..)
, DiscreteTime(..), SomeTime(..), someTimeMinutes
, Timetable(..), TimetableM(..)

) where

import Agent.Abstract

import Data.Typeable
import Data.Function (on)

import Data.Ix (Ix)
import Data.Set (Set)

\end{code}
%endif


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
                           classGroup      :: c -> AgentRef
                           classProfessor  :: c -> AgentRef
                           classRoom       :: c -> AgentRef
                           classNumber     :: c -> Word

class (AbstractClass c, DiscreteTime time) =>
  ConcreteClass c time | c -> time
    where  classDay     :: c -> Day
           classBegins  :: c -> time
           classEnds    :: c -> time

data Class      = forall c time  . ConcreteClass c time  => Class c
data SomeClass  = forall c       . AbstractClass c       => SomeClass c

instance Show Class      where show (Class c)      = show c
instance Show SomeClass  where show (SomeClass c)  = show c

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

\red{???}

One should distinguish the resulting timetables, shown in figure
\ref{fig:timetables} and the timetable, held by an agent
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
