\documentclass{article}

%include polycode.fmt

\usepackage{subcaption, hyperref}
\usepackage[english]{babel}
\usepackage[inline, shortlabels]{enumitem}

\usepackage{showframe}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if False
\begin{code}

module Document where

import Data.Ix

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


\section{University Classes}

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
 
class (DiscreteTime time) => Timetable tt x time  |  tt  -> time
                                                  ,  x   -> time
  where  classesOn  :: tt -> Day   -> [x]
         classesAt  :: tt -> time  -> [(Day, x)]
         
         classAt    :: tt -> Day -> time -> Maybe x
         

class (Timetable tt x time) => TimetableM tt m x time
  where  putClass :: tt -> 
  

\end{code}

\end{document}



%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% eval: (haskell-indentation-mode)
%%% eval: (haskell-session-change)
%%% eval: (load (concat (file-name-as-directory (projectile-project-root)) "publish-pdf.el"))
%%% End: