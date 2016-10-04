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

module Agent.Controller (

  AgentStatus(..), AgentStatus'

, Controller(..)
, ChildController(..), SomeChildController(..), ControllerImpl
, ParentController(..), SomeParentController(..), RootController
, ControllerNotification(..)

, controllerManagerDescriptor , RootControllerDescriptor(..)
, newChildController, newRootController

, ControlledAgentsDescriptor(..)
, ControllerSystemDescriptor(..), createControllerSystem
, NegotiationSysCtrl(..)

, CManagerState(..)
, controllerAct

, module Export

) where

  import Agent
  import Agent.Manager              as Export
  import Agent.Manager.CreateAgent  as Export

  import Data.Typeable
  import Data.Maybe (maybeToList, fromJust)

  import Data.Map (Map)
  import qualified Data.Map as Map

  import Control.Exception
  import Control.Arrow
  import Control.Concurrent (threadDelay)
  import Control.Concurrent.STM

\end{code}
%endif


The negotiation is created and monitored by the \emph{controller(s)}, that
may be composed into hierarchical structure.

> class Controller c where

\begin{itemize}
\item Creates agents from descriptors, returning the corresponding references.
      All agents would belong to the same execution role.

>  type NegotiationResult c :: *
>  type AgentsStatus c :: *
>  -- type AgentRunRole c :: *
>  newAgents  :: forall states res ag s . ( Typeable ag, Typeable s, Show s
>                                         , AgentCreate (AgentDescriptor states res) ag )
>             => c  -> [CreateAgent states res ag s]
>                   -> IO [AgentWithStatus c]

\item Guards all the created agents.

>  negotiatingAgents :: c -> IO [AgentWithStatus c]

\item Monitors agent's status.

>  monitorStatus   :: c  -> IO (Maybe (ControllerNotification c))
>  monitorStatus'  :: c  -> [(AgentFullRef, AgentsStatus c)]
>                        -> Maybe (ControllerNotification c)

\end{itemize}

\begin{code}

  data ControllerNotification c  = AllWaiting
                                 | AllLocked         [(AgentFullRef, NegotiationResult c)]
                                 | AgentsTerminated  [(AgentFullRef, SomeException)]
    deriving Typeable

  instance (Show (NegotiationResult c)) => Show (ControllerNotification c)
    where  show AllWaiting              = "AllWaiting"
           show (AllLocked ls)          = "AllLocked " ++ show ls
           show (AgentsTerminated ls)   = "AgentsTerminated " ++ show ls

  type AgentWithStatus c = (AgentFullRef, TVar (AgentsStatus c))

\end{code}



A child controller controller should notify the parent controller.

> class ChildController c where
>   parentController  :: c -> SomeParentController
>   notifyParentCtrl  :: forall msg . Message msg => c -> msg -> IO ()
>   notifyParentCtrl c = controllerNotification $ parentController c

It must execute parent's orders.

>   orderController  :: forall msg . Message msg => c -> msg -> IO ()
>   askController    :: forall msg resp . ( Message msg, Message resp
>                                         , ExpectedResponse msg ~ resp
>                                         ) => c -> msg -> IO resp

> data SomeChildController = forall ch . ChildController ch =>
>                            SomeChildController ch
> instance ChildController SomeChildController where
>    parentController  (SomeChildController ch) = parentController ch
>    orderController   (SomeChildController ch) = orderController ch
>    askController     (SomeChildController ch) = askController ch

A parent controller must:

> class ParentController c where

\begin{itemize}
  \item have a list of children;

>   controllerChildren :: c -> IO [SomeChildController]

  \item receive notification messages.

>   controllerNotification :: forall msg . Message msg => c -> msg -> IO ()

\end{itemize}

> data SomeParentController =  forall p . ParentController p =>
>                              SomeParentController p
> instance ParentController SomeParentController where
>    controllerChildren (SomeParentController p) = controllerChildren  p
>    controllerNotification (SomeParentController p) = controllerNotification p








Agent's status represents it's execution state at the moment.



\begin{code}

  data AgentStatus res  =  Initialized
                        |  Negotiating
                        |  Waiting res
                        |  Locked  res
                        |  Terminated (SomeException) deriving Show

  isWaiting (Waiting _)  = True
  isWaiting _            = False

  isLocked (Locked _)  = True
  isLocked _           = False

  getLocked (Locked l) = l

  isTerminated (Terminated _)  = True
  isTerminated _               = False

  mbTerminated (Terminated ex)  = Just ex
  mbTerminated _                = Nothing


  type AgentStatus' res = TVar (AgentStatus res)


\end{code}






Controller implementation.

Controller's state --- \verb|CManagerState| ---  is an instance of 'Controller', but only type
type instances are defined for it. Therefore no \verb|Controller|'s method should be used on
\verb|CManagerState|. It has full \verb|ChildController| and \verb|AgentsManager| capabilities.

\begin{code}

  data CManagerState res  = forall m ctrl . ( AgentsManager m (AgentStatus' res)
                                            , AgentsManagerOps m
                                            , Controller ctrl, Typeable ctrl
                                            , ChildController ctrl
                                            , Show (NegotiationResult ctrl)
                                            )
                          => CManagerState  {  selfManager  :: m
                                            ,  selfCtrl     :: ctrl
                                            ,  selfStatus   :: AgentStatus' res
                                            }

  instance Show (CManagerState res) where show CManagerState{} = "*ControllerManager State*"

  instance AgentsManager (CManagerState res) (AgentStatus' res) where
    newAgentsManager = undefined
    listAgents CManagerState{selfManager}         = listAgents selfManager
    listAgentStates CManagerState{selfManager}    = listAgentStates selfManager
    registerAgent CManagerState{selfManager}      = registerAgent selfManager
    unregisterAgent CManagerState{selfManager}    = unregisterAgent selfManager
    createWithManager CManagerState{selfManager}  = createWithManager selfManager

  instance ChildController (CManagerState res) where
    parentController CManagerState{selfCtrl}  = parentController selfCtrl
    orderController CManagerState{selfCtrl}   = orderController selfCtrl
    askController CManagerState{selfCtrl}     = askController selfCtrl

  instance Controller (CManagerState res) where
    type NegotiationResult (CManagerState res) = res
    type AgentsStatus (CManagerState res) = AgentStatus' res


  data ControllerImpl r res = ControllerImpl {
   controllerMA       :: ManagerAgent (CManagerState res) res,
   controlledRole     :: r,
   parentController_  :: SomeParentController
   }

  instance ( Typeable r, Typeable res, EmptyResult res ) =>
    Controller (ControllerImpl r res) where

    type NegotiationResult (ControllerImpl r res)  = res
    type AgentsStatus (ControllerImpl r res)       = AgentStatus res
--    type AgentRunRole (ControllerImpl r res)       = r

    negotiatingAgents  = fmap (map (second selfStatus)) . listAgentStates . controllerMA
    newAgents ctrl ds  = sequence $ ds >>=
        \d -> return $ do
            AgentsCreated ags <- controllerMA ctrl `ask` CreateAgents ds
            return . fromJust $ cast ags -- @UNSAFE

    monitorStatus c = do  ags <- negotiatingAgents c
                          sts <-  atomically . sequence
                                  $ map (\(k,v) -> (,) k <$> readTVar v) ags
                          return $ monitorStatus' c sts

    monitorStatus' _ rss  | all isWaiting     sts  = Just AllWaiting
                   | all isLocked      sts  = Just locked
                   | any isTerminated  sts  = Just terminated
                   | otherwise              = Nothing
     where  sts         = map snd rss
            locked      = AllLocked $ map (second getLocked) rss
            terminated  = AgentsTerminated $ do
                             (r, s) <- rss
                             maybeToList  . fmap ((,) r)
                                                      $ mbTerminated s

  instance (Typeable res) => ChildController (ControllerImpl r res) where
    parentController = parentController_
    orderController ctrl  = send  $ controllerMA ctrl
    askController ctrl    = ask   $ controllerMA ctrl




  data RootController = RootController {
    rootAgent     :: AgentFullRef,
    rootChildren  :: TVar (Map AnyRole SomeChildController)
    }

  instance ParentController RootController where
    controllerChildren = fmap Map.elems . readTVarIO . rootChildren
    controllerNotification  = send . rootAgent


\end{code}











Controller and underlying entities creation.

\begin{code}

  newChildController descr r parent = do
    ma <- newAgentsManagerAgent descr
    return ControllerImpl
      {  controllerMA       = ma
      ,  controlledRole     = r
      ,  parentController_  = parent
      }

  controllerManagerDescriptor  :: Bool
                               -> AgentBehavior s
                               -> IO (ManagerAgentDescriptor s)
  controllerManagerDescriptor debug behaviour = do
    cnt   <- newTVarIO 0

    return ManagerAgentDescriptor
      {  managerBehaviour  = behaviour
      ,  aManagerIdPrefix_ = "ChildController-"
      ,  aManagerCount_ = cnt
      ,  debugManager = debug
      }

-- managerBehaviour  = AgentBehavior (AgentActRepeat controllerAct waitTime)
--                           $ AgentHandleMessages
--                                (\_ m -> selectMessageHandler
--                                    [  handleStart undefined, handleStop undefined
--                                    ,  handleCreateAgents m ])
--                                (\_ m -> selectResponse [ responseCreateAgents m ])

  controllerAct _ s  = maybe (return ()) (notifyParentCtrl s)
                     =<< monitorStatus s

  instance Show (TVar (AgentStatus res)) where
    show _ = "TVar AgentStatus"


  data RootControllerDescriptor res = RootControllerDescriptor
    {  rootControllerIdPrefix   :: String
    ,  rootControllerCount      :: TVar Int
    ,  rootControllerBehaviour  :: AgentBehavior (TVar (Map AnyRole SomeChildController))
    ,  rootControllerDebug      :: Bool
    ,  rootNoResult             :: res
    }

  data NegotiationSysCtrl result = NegotiationSysCtrl {
    negotiationBegan   :: TMVar Millis,
    negotiationEnded   :: TMVar Millis,
    negotiationResult  :: TMVar [result],

    rootController     :: RootController
    }

  newRootController  (RootControllerDescriptor pref cVar behaviour debug noRes)
                     childrenDescrs
    = do
      chVar <- newTVarIO Map.empty
      let  rootDescriptor = AgentDescriptor{
              newAgentStates  = return chVar,
              nextAgentId     = const $ AgentId <$>
                                  do  c <- atomically $ do  cVar `modifyTVar` (+1)
                                                            readTVar cVar
                                      return $ pref ++ show c,
              agentDefaultBehaviour = behaviour,
              noResult = noRes,
              debugAgent = debug
            }
      (_ :: AgentRunOfRole AgentsManagerRole, ref) <- createAgent rootDescriptor

      let  root   = RootController ref chVar
           root'  = SomeParentController root

      children <- sequence $ do  (ControlledAgentsDescriptor d r chs) <- childrenDescrs
                                 let ch = do  ctrl <- newChildController d r root'
                                              ctrl `askController` CreateAgents chs
                                              return $ SomeChildController ctrl
                                 return $ (,) (AnyRole r) <$> ch
      atomically $ do  chVar `writeTVar` Map.fromList children
                       began   <- newEmptyTMVar
                       ended   <- newEmptyTMVar
                       result  <- newEmptyTMVar
                       return NegotiationSysCtrl {
                          negotiationBegan   = began,
                          negotiationEnded   = ended,
                          negotiationResult  = result,
                          rootController     = root
                       }

\end{code}











Controllers creation.

\begin{code}

  data ControlledAgentsDescriptor  = forall states r ag res exState . (
                                        Typeable r, Show r, RoleIx r
                                      , Typeable res, EmptyResult res
                                      , Typeable ag, Typeable exState, Show exState
                                      , AgentCreate (AgentDescriptor states res) ag
                                   ) =>
       ControlledAgentsDescriptor  (ManagerAgentDescriptor (CManagerState res))
                                   r
                                   [CreateAgent states res ag exState]



  data ControllerSystemDescriptor ms result =
       ControllerSystemDescriptor  (RootControllerDescriptor result)
                                   [ControlledAgentsDescriptor]


  createControllerSystem  :: (Typeable result) =>
                          ControllerSystemDescriptor ms result -> IO (NegotiationSysCtrl result)
  createControllerSystem (ControllerSystemDescriptor rd ds) = newRootController rd ds



\end{code}



