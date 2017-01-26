%if False
\begin{code}
module AUCSP.Test.TestData01 where

  import AUCSP.Classes
  import AUCSP.Contexts
  import AUCSP.NegotiationStates

  import Data.Set (Set)
  import qualified Data.Set as Set

\end{code}
%endif




The test data consists of \textbf{1 group}, \textbf{2 disciplines}
\textbf{2 professors}, and \textbf{2 classrooms}.

Disciplines and requirements:

> dMath    = Discipline  "Math I"       60    Set.empty
> dPhys    = Discipline  "Physics I"    60  $ Set.singleton physLab
> physLab  = Requirement "Physics Lab"

Initial thresholds: \red{TODO: set values}

> prefThresh  = 0.5
> extThresh   = 0.7

Group has 15 students that need both classes. No obligations or preferences defined.

> groupCtx = mkContexts  (GroupCapabilities 15 [dMath, dPhys])
>                        (Obligations [] [])
>                        (Preferences' [] [] prefThresh)
>                        (External' extThresh)

Professor I can teach \emph{Math I}.

> profCtx1 = mkContexts  (FullTimeProfCapabilities [dMath])
>                        (Obligations [] [])
>                        (Preferences' [] [] prefThresh)
>                        (External' extThresh)

Professor II can teach \emph{Physics I}.

> profCtx2 = mkContexts  (FullTimeProfCapabilities [dPhys])
>                        (Obligations [] [])
>                        (Preferences' [] [] prefThresh)
>                        (External' extThresh)

Classroom I can hold 30 students, but has no special equipment.

> roomCtx1 = mkContexts  (ClassroomCapabilities 30 [])
>                        (Obligations [] [])
>                        (Preferences' [] [] prefThresh)
>                        (External' extThresh)

Classroom II is a physics laboratory, equipped for 20 students.

> roomCtx2 = mkContexts  (ClassroomCapabilities 20 [physLab])
>                        (Obligations [] [])
>                        (Preferences' [] [] prefThresh)
>                        (External' extThresh)

