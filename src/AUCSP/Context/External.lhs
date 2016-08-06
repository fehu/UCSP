
%if False
\begin{code}

module AUCSP.Context.External(

  External(..)

, KnownAgent, askKnownAgent

, OpinionRel(..), OpinionAbout(..)
, MyOpinion(..), extractMyOpinion

) where

import AUCSP.Classes
import AUCSP.NegotiationRoles
import AUCSP.Coherence
import AUCSP.Context
import AUCSP.Context.Capabilities (Capabilities)
import AUCSP.Context.InUnitInterval
import GenericAgent

import qualified AUCSP.Context.Combine as Combine

import Data.Typeable (Typeable, cast)
import Data.Function (on)
import Data.IORef
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)

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
of agent $\mathrm{ag}^\mathrm{role}_i$ on class $c_i$, of which consists
the proposal in question $p_k$. They are combined using $\product$ operation.

\begin{code}

data KnownAgent = forall r a . KnownAgent {
  knownAgentRef           :: AgentRef,
  knownAgentRole          :: r,
  knownAgentCapabilities  :: [Capabilities r a]
  }
  deriving (Typeable)

-- askKnownAgent ::  ( MessageT msg a
--                   , MessageT (ExpectedResponse1 msg) a
--                   , Fractional a)
--               => KnownAgent a
--               -> msg a
--               -> IOMaybe (ExpectedResponse1 msg a)

askKnownAgent ::  ( Message msg, Message (ExpectedResponse msg))
              => KnownAgent
              -> msg
              -> IOMaybe (ExpectedResponse msg)
askKnownAgent knownAg message =
    do  resp <- knownAgentRef knownAg `ask` message
        return $ cast resp

instance Eq KnownAgent where
  (==) = (==) `on` knownAgentRef

instance Ord KnownAgent where
  compare = compare `on` knownAgentRef

instance Show KnownAgent where
    show KnownAgent{knownAgentRef=ref} = "KnownAgent " ++ show (show ref)

instance InformationPiece KnownAgent
    where type IScope KnownAgent = Personal

-- -----------------------------------------------

data External a = External {
    knownAgents        :: IORef [KnownAgent]
  , externalThreshold  :: IORef a
  }


instance (Typeable a, Num a) => Context External a where
  contextName _       = "External"
  contextInformation  = fmap (fromNodes . map Information)
                      . readIORef . knownAgents
  contextRelations r  = return [ RelBinIO OpinionRel ]
  contextThreshold    = readIORef . externalThreshold
  combineBinRels      = Combine.binRelsProduct
  combineWholeRels    = undefined -- None
  combineRels         = undefined -- None

-- -----------------------------------------------

data OpinionRel a = OpinionRel

newtype OpinionAbout = OpinionAbout Class deriving (Typeable, Show)
data MyOpinion = forall a . (Show a, Typeable a) =>
     MyOpinion (Maybe (InUnitInterval a)) deriving Typeable

instance Show MyOpinion where show (MyOpinion x) = "MyOpinion (" ++ show x ++ ")"

type instance ExpectedResponse OpinionAbout = MyOpinion

extractMyOpinion (MyOpinion mbOpinion) = cast =<< mbOpinion

-- -----------------------------------------------

instance Functor OpinionRel where fmap _ = const OpinionRel

instance InformationRelation OpinionRel where
  relationName _  = "Opinion"
  coerceRelation  = coerce

instance BinaryIORelation OpinionRel where
  binRelIOValue rel a b = fromMaybe (return Nothing)
    $ do  knownAg  <- collectInf a
          class'   <- collectInf b
          return $ do  resp <- askKnownAgent knownAg (OpinionAbout class')
                       return . fmap fromUnitInterval $ extractMyOpinion =<< resp

\end{code}

