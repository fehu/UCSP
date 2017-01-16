
%if False
\begin{code}

module AUCSP.Context.External(

  External(..)

, KnownAgent(..), askKnownAgent
, SomeKnownAgent(..), knownAgentRef'
, KnownAgents(..), emptyKnownAgents, flattenKnownAgents
, findKnownAgent, getKnownAgent
, getKnownGroup, getKnownProfessor, getKnownClassroom

, OpinionRel(..), OpinionAbout(..)
, MyOpinion(..), extractMyOpinion



) where

import Agent.SomeAgent

import AUCSP.Classes
import AUCSP.NegotiationRoles as Role
import AUCSP.Coherence
import AUCSP.Context
import AUCSP.Context.Capabilities (Capabilities)
import AUCSP.Context.InUnitInterval

import qualified AUCSP.Context.Combine as Combine

import Data.Typeable (Typeable, cast)
import Data.Function (on)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (find)

import Control.Arrow ( (&&&) )

\end{code}
%endif

\subsubsection{External}
 \label{subsec:context-external}

External contexts take into account the \emph{opinions} of the
agents that are referenced by the solution candidate.
It is responsible for \emph{common goal} assessment.
The assessment must be \emph{objective} --- it must give no preference
to agent's own interests.

The \emph{context-specific information} consists of references to the known
agents with cached information about their capabilities.

There is a single binary relation in this context --- \emph{opinion}
of agent $\mathrm{ag}^\mathrm{role}_i$ about class $c_i$, of which consists
the proposal in question $p_k$. They are combined using $\prod$ operation.

\begin{code}

-- data KnownAgent r a = KnownAgent {
--   knownAgentRef           :: AgentRef,
--   knownAgentRole          :: r,
--   knownAgentCapabilities  :: Capabilities r a
--   }
--   deriving (Typeable)
--
-- data SomeKnownAgent =  forall r a . ( Typeable r, Typeable a) =>
--                        SomeKnownAgent (KnownAgent r a)
--
-- knownAgentRef' (SomeKnownAgent k) = knownAgentRef k
--
-- askKnownAgent ::  ( Message msg, Message (ExpectedResponse msg))
--               => SomeKnownAgent
--               -> msg
--               -> IOMaybe (ExpectedResponse msg)
-- askKnownAgent knownAg message =
--     do  resp <- knownAgentRef' knownAg `ask` message
--         return $ cast resp
--
-- instance Eq (KnownAgent r a)  where (==) = (==) `on` knownAgentRef
-- instance Eq SomeKnownAgent    where (==) = (==) `on` knownAgentRef'
--
-- instance Ord (KnownAgent r a)  where compare = compare `on` knownAgentRef
-- instance Ord SomeKnownAgent    where compare = compare `on` knownAgentRef'
--
-- instance Show (KnownAgent r a) where
--     show KnownAgent{knownAgentRef=ref} = "KnownAgent " ++ show ref
-- instance Show SomeKnownAgent where
--     show (SomeKnownAgent k) = "SomeKnownAgent " ++ show (knownAgentRef k)
--
--
-- instance (Typeable r, Typeable a) => InformationPiece (KnownAgent r a)
--     where type IScope (KnownAgent r a)  = Personal
-- instance InformationPiece SomeKnownAgent
--     where type IScope SomeKnownAgent    = Personal



instance InformationPiece SomeAgent
  where type IScope SomeAgent = Personal


-- instance (AgentOfRole r) => InformationPiece (KnownAgent r)
--   where type IScope (KnownAgent r)  = Personal

-- -----------------------------------------------

data External a = External {
    knownAgents        :: KnownAgents
  , externalThreshold  :: IO a
  }


instance (Typeable a, Num a) => Context External a where
  contextName _       = "External"
  contextInformation  = fmap (fromNodes . map Information)
                      . flattenKnownAgents . knownAgents
  contextRelations r  = return [ RelBinIO OpinionRel ]
  contextThreshold    = externalThreshold

  type AssessmentDetails External = ExternalDetails

  combineBinRels = Combine.binRelsProduct $
        ExternalDetails . concatMap (
            mapMaybe  (extractDetails . relBetweenDetails) . snd
        )

  combineWholeRels    = undefined -- None
  combineRels         = undefined -- None

  noAssessmentDetails _ = ExternalDetails []

-- -----------------------------------------------

data OpinionRel a = OpinionRel deriving Typeable

-- newtype OpinionAbout = OpinionAbout Class deriving (Typeable, Show)

type instance RelationDetails OpinionRel = OpinionRelDetail

-- data MyOpinion = forall a . (Show a, Typeable a, Fractional a) =>
--      MyOpinion a deriving Typeable
--
-- instance Show MyOpinion where show (MyOpinion x) = "MyOpinion (" ++ show x ++ ")"
--
-- type instance ExpectedResponse OpinionAbout = MyOpinion
--
-- extractMyOpinion (MyOpinion mbOpinion) = cast mbOpinion

-- -----------------------------------------------

instance Functor OpinionRel where fmap _ = const OpinionRel

instance InformationRelation OpinionRel where
  relationName _  = "Opinion"
  coerceRelation  = coerce

-- instance BinaryIORelation OpinionRel where
--   binRelIOValue rel a b = maybe (return Nothing)
--                                 (fmap (fmap (opinionVal &&& id)))
--     $ do  knownAg  <- collectInf a
--           class'   <- collectInf b
--           return $ do  resp <- askKnownAgent knownAg (OpinionAbout class')
--                        let mbA = fmap fromUnitInterval $ extractMyOpinion =<< resp
--                        return $ fmap (OpinionRelDetail class' knownAg) mbA

-- -----------------------------------------------

data OpinionRelDetail a = OpinionRelDetail{
    opinionAbout  :: Class,
    -- opinionOf     :: SomeKnownAgent,
    opinionVal    :: a
    }

newtype ExternalDetails a = ExternalDetails [OpinionRelDetail a]

-- -----------------------------------------------

data KnownAgents = KnownAgents{
  knownGroups       :: IO [KnownAgent Role.Group],
  knownProfessors   :: IO [KnownAgent Role.Professor],
  knownClassrooms   :: IO [KnownAgent Role.Classroom]
  }

flattenKnownAgents (KnownAgents gsv psv rsv) = do
    gs <- map someAgent <$> gsv
    ps <- map someAgent <$> psv
    rs <- map someAgent <$> rsv
    return $ gs ++ ps ++ rs

emptyKnownAgents = atomically $ do
    groups  <- newTVar []
    profs   <- newTVar []
    rooms   <- newTVar []
    return $ KnownAgents groups profs rooms

findKnownAgent var ref = do  ags <- readTVarIO var
                             return $ find ((ref ==) . knownAgentRef) ags

getKnownAgent v r  = findKnownAgent v r
                   >>= maybe (fail $ "agent not known: " ++ show r) return


getKnownGroup kn c     = getKnownAgent  (knownGroups kn)
                                        (simpleRef $ classGroup c)

getKnownProfessor kn c = getKnownAgent  (knownProfessors kn)
                                        (simpleRef $ classProfessor c)

getKnownClassroom kn c = getKnownAgent  (knownClassrooms kn)
                                        (simpleRef $ classRoom c)


\end{code}
