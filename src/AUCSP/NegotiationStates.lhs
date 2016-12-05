%if standalone
\documentclass{article}

%include src/Document/format.fmt
%include polycode.fmt
%include forall.fmt

\usepackage[english]{babel}
\usepackage[inline, shortlabels]{enumitem}

\usepackage{showframe}

\begin{document}
%endif

%if False
\begin{code}
module AUCSP.NegotiationStates (

  States(..), mkStates
, groupStates, professorStates, classroomStates

, mkContexts, ContextsHolder(..)
, Preferences'(..), External'(..)

, RoleDef(..)

) where

import Agent.AgentSystem (AgentStatus(Initialized), AgentStatus')

import AUCSP.Contexts
import AUCSP.Classes
import AUCSP.Coherence
import AUCSP.NegotiatingAgent
import qualified AUCSP.NegotiationRoles as Role

import Data.Typeable

import Control.Concurrent.STM

\end{code}
%endif


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\begin{code}

data ContextsHolder r a = ContextsHolder{
  ctxCapabilities_  :: Capabilities r a,
  ctxBeliefs_       :: Beliefs a,
  ctxObligations_   :: Obligations a,
  ctxPreferences_   :: Preferences a,
  ctxExternal_      :: External a,

  preferencesThreshold'  :: TVar a,
  externalThreshold'     :: TVar a
  }

data RoleDef r a = RoleDef{
  role_             :: r,
  counterpartsOf_   :: forall cl . AbstractClass cl => States r a -> cl -> IO [SomeKnownAgent]
  }


data States r a = States {
  statusState  :: AgentStatus' SomeCandidate,
  decider_     :: DeciderUCSP a,
  roledef_     :: RoleDef r a,
  ctx_         :: ContextsHolder r a
  }
  deriving Typeable


instance (Num a) => Contexts (States r a) a where
    type ContextsRole (States r a) = r
    agentRole            = role_ . roledef_
    capabilitiesContext  = ctxCapabilities_ . ctx_
    beliefsContext       = ctxBeliefs_ . ctx_
    obligationsContext   = ctxObligations_ . ctx_
    preferencesContext   = ctxPreferences_ . ctx_
    externalContext      = ctxExternal_ . ctx_
    counterpartsOf s     = counterpartsOf_ (roledef_ s) s

instance (Num a) => AgentStates (States r a) a where
    decider = decider_

rCounterparts l s c = mapM ($ (getKnownAgents s, c)) l

mkStates roledef ctx decider = do
    status  <- newTVarIO Initialized
    return  $ States status decider roledef ctx

groupStates  :: (Num a, Typeable a) =>
                ContextsHolder Role.Group a
             -> DeciderUCSP a
             -> IO (States Role.Group a)
groupStates =  mkStates $ RoleDef Role.Group $
               rCounterparts [ fmap SomeKnownAgent . uncurry getKnownProfessor
                             , fmap SomeKnownAgent . uncurry getKnownClassroom ]

professorStates  :: (Num a, Typeable a) =>
                    ContextsHolder Role.Professor a
                 -> DeciderUCSP a
                 -> IO (States Role.Professor a)
professorStates =  mkStates $ RoleDef Role.FullTimeProfessor $
                   rCounterparts [ fmap SomeKnownAgent . uncurry getKnownGroup
                                 , fmap SomeKnownAgent . uncurry getKnownClassroom ]

classroomStates  :: (Num a, Typeable a) =>
                    ContextsHolder Role.Classroom a
                 -> DeciderUCSP a
                 -> IO (States Role.Classroom a)
classroomStates =  mkStates $ RoleDef Role.Classroom $
                   rCounterparts [ fmap SomeKnownAgent . uncurry getKnownGroup
                                 , fmap SomeKnownAgent . uncurry getKnownProfessor ]


\end{code}


\begin{code}

data Preferences' a = Preferences'  [Information]
                                    [IRelation (InUnitInterval a)]
                                    a -- threshold

toPreferences (Preferences' i r th) = do ref <- newTVarIO th
                                         return (ref, Preferences i r $ readTVarIO ref)

data External' a = External' a -- threshold

toExternal (External' th) = do  known  <- emptyKnownAgents
                                ref    <- newTVarIO th
                                return (ref, External known $ readTVarIO ref)

mkContexts capabilities obligations preferences external = do
    beliefs <- Beliefs <$> newTVarIO (fromNodes [])
    (prefsTh, prefs) <- toPreferences preferences
    (extTh, ext) <- toExternal external
    return $ ContextsHolder  capabilities beliefs
                             obligations prefs
                             ext  prefsTh  extTh

\end{code}






%if standalone
\end{document}
%endif


%%% local variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-build-standalone-flag: t
%%% eval: (haskell-indentation-mode)
%%% eval: (interactive-haskell-mode)
%%% End:

