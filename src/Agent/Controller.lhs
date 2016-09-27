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

, managerAgentDescriptor , RootControllerDescriptor(..)
, newChildController, newRootController

, ControlledAgentsDescriptor(..), Millis
, ControllerSystemDescriptor(..), createControllerSystem

, module Agent.Manager

) where

  import Agent
  import Agent.Manager

--  import AUCSP.Context

  import Data.Typeable
  import Data.Maybe (maybeToList, fromJust)

  import Control.Exception
  import Control.Arrow
  import Control.Concurrent (threadDelay)
  import Control.Concurrent.STM

\end{code}
%endif


Agent's status represents it's execution state at the moment.


\begin{code}

  data AgentStatus  =  Initialized
                    |  Negotiating
                    |  Waiting SomeCandidate
                    |  Locked  SomeCandidate
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


  type AgentStatus' = TVar AgentStatus
  type AgentWithStatus = (AgentFullRef, AgentStatus')

  data ControllerNotification  = AllWaiting
                               | AllLocked         [(AgentFullRef, SomeCandidate)]
                               | AgentsTerminated  [(AgentFullRef, SomeException)]
    deriving (Typeable, Show)

\end{code}


The negotiation is created and monitored by the \emph{controller(s)}, that
may be composed into hierarchical structure.

> class Controller c where

\begin{itemize}
\item Creates agents from descriptors, returning the corresponding references.
      All agents would belong to the same execution role.

>  newAgents  :: forall states . (Typeable states)
>             => c -> [AgentDescriptor states] -> IO [AgentWithStatus]
>  type AgentRunRole c :: *

\item Guards all the created agents.

>  negotiatingAgents :: c -> IO [AgentWithStatus]

\item Monitors agent's status.

>  monitorStatus   :: c -> IO (Maybe ControllerNotification)
>  monitorStatus'  :: c -> [(AgentFullRef, AgentStatus)] -> Maybe ControllerNotification

\end{itemize}

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







Controller implementation.

\begin{code}

  data ControllerImpl r = ControllerImpl {
   controllerMA       :: ManagerAgent AgentStatus',
   controlledRole     :: r,
   parentController_  :: SomeParentController
   }

  instance (Typeable r) => Controller (ControllerImpl r) where
    negotiatingAgents  = listAgentStates . controllerMA

    type AgentRunRole (ControllerImpl r) = r
    newAgents ctrl ds  = sequence $ ds >>=
        \d -> return $ do
            let create = createAgent <$> ds
                       :: [IO (AgentRunOfRole r, AgentFullRef)]
            AgentsCreated ags <- controllerMA ctrl `ask` CreateAgents create
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

  instance ChildController (ControllerImpl r) where
    parentController = parentController_
    orderController ctrl  = send  $ controllerMA ctrl
    askController ctrl    = ask   $ controllerMA ctrl




  data RootController = RootController {
    rootAgent     :: AgentFullRef,
    rootChildren  :: TVar [SomeChildController]
    }

  instance ParentController RootController where
    controllerChildren = readTVarIO . rootChildren
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

  type Millis = Int -- Milliseconds

  managerAgentDescriptor  :: (Typeable r)
                          => Millis
                          -> IO (ControllerImpl r)
                          -> ExtractStateFunc AgentStatus'
                          -> IO ManagerAgentDescriptor
  managerAgentDescriptor waitTime getCtrl exState = do
    ctrl  <- getCtrl
    cnt   <- newTVarIO 0

    let  manager = controllerMA ctrl

    return ManagerAgentDescriptor
      {  managerAct_ = const $ controllerAct waitTime ctrl
      ,  aManagerIdPrefix_ = "ChildController-"
      ,  aManagerCount_ = cnt
      ,  amHandleMessage_ =  [ handleStart undefined, handleStop undefined
                             , handleCreateAgents manager exState ]
      ,  amRespondMessage_ = [responseCreateAgents manager exState]
      }

  controllerAct waitTime ctrl  = do maybe (return ()) (notifyParentCtrl ctrl)
                                           =<< monitorStatus ctrl
                                    threadDelay waitTime

  instance Show (TVar AgentStatus) where
    show _ = "TVar AgentStatus"


  data RootControllerDescriptor = RootControllerDescriptor
    {  rootControllerIdPrefix  :: String
    ,  rootControllerCount     :: TVar Int
    ,  rootControllerAct       :: forall i . AgentInnerInterface i => i -> IO ()
    ,  rootControllerHandle    :: forall msg i . ( AgentInnerInterface i
                                                 , Message msg )
                               => i -> [msg -> Maybe (IO ())]
    }


  newRootController  (RootControllerDescriptor pref cVar act' handle)
                     childrenDescrs
    = do
      let  rootDescriptor = AgentDescriptor{
              newAgentStates  = return (),
              nextAgentId     = const $ AgentId <$>
                                  do  c <- atomically $ do  cVar `modifyTVar` (+1)
                                                            readTVar cVar
                                      return $ pref ++ show c,
              agentBehaviour  = AgentBehavior{
                act             = \i _ -> act' i,
                handleMessages  = AgentHandleMessages{
                  handleMessage   = \i _ -> selectMessageHandler $ handle i,
                  respondMessage  = \i _ _ -> undefined
                  }
                }
            }
      (_ :: AgentRunOfRole AgentsManagerRole, ref) <- createAgent rootDescriptor
      chVar <- newTVarIO []

      let  root   = RootController ref chVar
           root'  = SomeParentController root

      children <- sequence $ do  (ControlledAgentsDescriptor d r chs) <- childrenDescrs
                                 return $ do  ctrl <- newChildController d r root'
                                              ctrl `askController` CreateAgents chs
                                              return $ SomeChildController ctrl
      atomically $ chVar `writeTVar` children
      return root

\end{code}











Controllers creation.

\begin{code}

  data ControlledAgentsDescriptor = forall r ag . (Typeable r, Typeable ag) =>
       ControlledAgentsDescriptor  ManagerAgentDescriptor
                                   r
                                   [IO (ag, AgentFullRef)]



  data ControllerSystemDescriptor =
       ControllerSystemDescriptor  RootControllerDescriptor
                                   [ControlledAgentsDescriptor]


  createControllerSystem  :: ControllerSystemDescriptor -> IO RootController
  createControllerSystem (ControllerSystemDescriptor rd ds) = newRootController rd ds



\end{code}



