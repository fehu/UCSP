
%if False
\begin{code}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module AUCSP.Agent.AgentState (

  AgentState(..)

) where

  import AUCSP.Contexts
  import AUCSP.Classes
  import AUCSP.NegotiationRoles

  import Data.Set (Set)
  import qualified Data.Set as Set

  import Control.Concurrent.STM

\end{code}
%endif

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

Agent's state include:

> data AgentState r a = AgentState{
>   contexts        :: Contexts r a,
>   bestCandidate   :: TVar (Maybe (Candidate a)),
>   classCorePool   :: TVar (Set SomeClass),
>   knownAgentsVars :: MutableKnownAgents,
>   knownAgents'    :: KnownAgents
> }


> data MutableKnownAgents = MutableKnownAgents{
>   varKnownGroups     :: TVar [KnownAgent Group],
>   varKnownProfessors :: TVar [KnownAgent Professor],
>   varKnownClassrooms :: TVar [KnownAgent Classroom]
>  }

> mkKnownAgents :: MutableKnownAgents -> KnownAgents
> mkKnownAgents a = KnownAgents (readTVarIO $ varKnownGroups a)
>                               (readTVarIO $ varKnownProfessors a)
>                               (readTVarIO $ varKnownClassrooms a)

> newMutableKnownAgents = atomically $ do  groups  <- newTVar []
>                                          profs   <- newTVar []
>                                          rooms   <- newTVar []
>                                          return $ MutableKnownAgents
>                                                   groups profs rooms

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

> newAgentState = do  cxts      <- newContexts undefined  -- TODO
>                     bestVar   <- newTVarIO Nothing
>                     poolVar   <- newTVarIO Set.empty
>                     knownVar  <- newMutableKnownAgents
>                     return . AgentState cxts bestVar poolVar knownVar
>                            $ mkKnownAgents knownVar


> newContexts caps = return $ Contexts  caps
>                                       undefined -- Beliefs -- TODO
>                                       undefined
>                                       undefined
>                                       undefined
>                                       undefined








% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
