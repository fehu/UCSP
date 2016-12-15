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

module Agent.AgentSystem (


  module Export

) where

  import Control.Monad
  import Control.Concurrent.STM
  import Control.Exception (SomeException)

  import Data.Typeable
  import Data.Map (Map)
  import Data.Maybe (fromJust)
  import Data.Time.Clock

  import qualified Data.Map as Map

  import Agent as Export
  import Agent.AgentSystem.Controller as Export


  -- _local_DEBUG = True
  -- localDebug scope = when _local_DEBUG . putStrLn . (("[DEBUG] " ++ scope ++ ": ") ++)

\end{code}
%endif



\red{rewrite it}

An \emph{agent system} uses agent \emph{roles} to create control hierarchy.
Furthermore, roles serve as agents \emph{archetypes}, defining agents' states
structure and behavior traits.


> class AgentSystem sys res | sys -> res where
>   createAgentSystem :: IO sys

>   newAgents :: (AgentRoleDescriptor r res)  => sys
>                                             -> r
>                                             -> [RoleInput r]
>                                             -> IO [AgentFullRef]
>   listAgents       :: sys -> IO (Map AnyRole [AgentFullRef])
>   listAgentStates' :: sys -> IO (Map AnyRole (AgentStatusMap res))

>   startAgents :: sys -> IO ()
>   stopAgents  :: sys -> IO ()

>   terminateAgents       :: sys -> IO ()
>   terminateAgentsForce  :: sys -> IO ()

>   agentsStarted :: sys -> IO (Maybe UniversalTime)
>   agentsStopped :: sys -> IO (Maybe UniversalTime)

>   tryGetResult :: sys -> IO (Maybe (Either SomeException res))
>   waitResult   :: sys -> IO (Either SomeException res)


% >   mapAgents        :: (AgentFullRef -> IO a)   -> m -> IO [a]
% >   mapAgents_       :: (AgentFullRef -> IO ())  -> m -> IO ()
%
% >   mapAgentStates   :: ((AgentFullRef, s) -> IO a)   -> m -> IO [a]
% >   mapAgentStates_  :: ((AgentFullRef, s) -> IO ())  -> m -> IO ()
%
% >   foreachAgent     :: m -> (AgentFullRef -> IO a)   -> IO [a]
% >   foreachAgent_    :: m -> (AgentFullRef -> IO ())  -> IO ()
%
% >   foreachAgentState   :: m -> ((AgentFullRef, s)  -> IO a)   -> IO [a]
% >   foreachAgentState_  :: m -> ((AgentFullRef, s)  -> IO ())  -> IO ()
%
% >   mapAgents f   = mapM f   <=< listAgents
% >   mapAgents_ f  = mapM_ f  <=< listAgents
% >   mapAgentStates f   = mapM f   <=< listAgentStates
% >   mapAgentStates_ f  = mapM_ f  <=< listAgentStates
%
% >   foreachAgent   = flip mapAgents
% >   foreachAgent_  = flip mapAgents_
% >   foreachAgentState   = flip mapAgentStates
% >   foreachAgentState_  = flip mapAgentStates_




> class (RoleIx r, Show r, Typeable r, Typeable (RoleStates r)) =>
>   AgentRoleDescriptor r res | r -> res where
>     type RoleInput  r :: *
>     type RoleStates r :: *
>     roleStates     :: r -> RoleInput r   -> IO (RoleStates r)
>     roleStatesExt  :: r -> RoleStates r  -> AgentStatus' res
>     roleBehavior   :: r -> RoleInput r   -> AgentBehavior (RoleStates r)
>     nextRoleId     :: r -> IO AgentId

% >     roleStatesExt  :: r -> RoleStates r  -> AgentStatus' res

> data SomeRole res = forall r . ( AgentRoleDescriptor r res
>                                , RoleIx r ) => SomeRole r
>   deriving Typeable
> instance Show (SomeRole res) where show (SomeRole r) = show r

% > instance RoleIx (SomeRole res) where

> data RootRole res = RootRole

\verb|AgentSystem| creates an hierarchy, where all agents of the same role
are controlled by a unique \verb|ControllerLeaf|. The only \verb|ControllerNode|
is system's root.

\verb|AgentSystem| implementation:

\begin{code}

  data NegotiationSysCtrl result = NegotiationSysCtrl {
      _negotiationBegan   :: TMVar ExecTime,
      _negotiationEnded   :: TMVar ExecTime,
      _negotiationResult  :: TMVar [(AgentRef, result)],

      _negotiationDebug :: Bool,
      rootController     :: RootController result
  }

  data ExecTime = forall t . (Ord t, Show t) => ExecTime t
  instance Show ExecTime where show (ExecTime t) = show t

  type RootController res = AgentsOverseer (SomeRole res) res

  class NoResult res where noResult' :: res

  instance ( Typeable res, NoResult res
          --  , Typeable (AgentPosition (AgentsManager res))
          -- !!!!! TODO : `ControllerLeaf` has no 'position', only nodes !!!!!!!  TODO
            ) =>
    AgentSystem (NegotiationSysCtrl res) res where
      newAgents sys r ris = fmap (map fst) $
        newAgentsN (rootController sys) (SomeRole r)
        $ do  ri <- ris

              let  descr = AgentDescriptor
                             (roleBehavior r ri)
                             (roleStates r ri)
                             (nextRoleId r)
                             noResult'
                             (_negotiationDebug sys)

              return $ crAg r descr (roleStatesExt r)

      listAgents = undefined -- fmap () . listAgentStates'
      listAgentStates' = fetchAgents [] <=< listControllers . rootController
        where  fetchAgents acc [] = return $ Map.fromList acc
               fetchAgents acc (SomeController ctrl : ctrls) =
                 case cast ctrl
                  of Just (leaf :: AgentsManager res) -> do
                      ags <- controlledAgents leaf
                      let role' = case cast $ controllerPosition leaf
                                  of Just (SomeRole r) -> AnyRole  r
                      fetchAgents  ((role', ags) : acc)
                                   ctrls



  crAg :: ( Typeable r, Typeable states, Typeable res
          , Typeable (RoleStates r) ) =>
           r -> AgentDescriptor states res
             -> (RoleStates r -> AgentStatus' res)
             -> CreateAgent (AgentRunOfRole r) res
  crAg _ descr exState' = CreateAgent descr exState
    where exState = exState' . fromJust . extractAgentStates

\end{code}










%if standalone
\end{document}
%endif

%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-build-standalone-flag: t
%%% eval: (haskell-indentation-mode)
%%% eval: (interactive-haskell-mode)
%%% End:
