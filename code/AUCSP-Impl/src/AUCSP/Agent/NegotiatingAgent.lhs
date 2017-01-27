
%if False
\begin{code}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module AUCSP.Agent.NegotiatingAgent (

) where

  import AgentRole

  import AUCSP.NegotiationRoles
  import AUCSP.AgentsInterface
  import AUCSP.Context.Capabilities

  import AUCSP.Agent.AgentState

\end{code}
%endif

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

The roles, defined at \verb|AUCSP.NegotiationRoles|, require \verb|AgentRole| instances.

\begin{code}


  instance AgentRole Group where
    type RoleResult Group = NegotiationResult
    roleName = show
    type RoleState  Group = AgentState Group
    type RoleArgs   Group = RequitedData Group


  instance AgentRole Professor where
    type RoleResult Professor = NegotiationResult
    roleName = show
    type RoleState  Professor = AgentState Professor
    type RoleArgs   Professor = RequitedData Professor


  instance AgentRole Classroom where
    type RoleResult Classroom = NegotiationResult
    roleName = show
    type RoleState  Classroom = AgentState Group
    type RoleArgs   Classroom = RequitedData Group

\end{code}





% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

Negotiating agent implements \verb|AgentOfRole| from \verb|AgentsInterface|.
\verb|AgentOfRoleData| is defined at \verb|AUCSP.Context.Capabilities|.

> type NegAgentRef = AgentRef NegotiationResult

> instance AgentOfRoleRef  Group where type RoleRef  Group = NegAgentRef
> instance AgentOfRole Group where
>   data KnownAgent Group = KnownGroup NegAgentRef (CapabilitiesFor Group)
>   roleOf _ = Group
>   roleData (KnownGroup _ d) = d
>   roleRef  (KnownGroup r _) = r
>   roleAgentId = agentId . roleRef
> instance Show (KnownAgent Group) where
>   show = ("KnownGroup " ++) . show . roleAgentId


> instance AgentOfRoleRef Professor where
>   type RoleRef Professor = NegAgentRef
> instance AgentOfRole Professor where
>   data KnownAgent Professor = KnownProfessor  Professor
>                                               NegAgentRef
>                                              (CapabilitiesFor Professor)
>   roleOf   (KnownProfessor p _ _) = p
>   roleData (KnownProfessor _ _ d) = d
>   roleRef  (KnownProfessor _ r _) = r
>   roleAgentId = agentId . roleRef
> instance Show (KnownAgent Professor) where
>   show = ("KnownProfessor " ++) . show . roleAgentId


> instance AgentOfRoleRef Classroom where
>   type RoleRef Classroom = NegAgentRef
> instance AgentOfRole Classroom where
>   data KnownAgent Classroom = KnownRoom NegAgentRef (CapabilitiesFor Classroom)
>   roleOf _ = Classroom
>   roleData (KnownRoom _ d) = d
>   roleRef  (KnownRoom r _) = r
>   roleAgentId = agentId . roleRef
> instance Show (KnownAgent Classroom) where
>   show = ("KnownRoom " ++) . show . roleAgentId


% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

A negotiating agent of a given role requires:

> data RequitedData r = RequitedData {

\begin{itemize}
  \item Unique name.

>   uniqueAgentName :: String,

  \item ???

  \item Debug flag.

>   debugAgent :: Bool

\end{itemize}

> }



% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

Negotiation result is \red{...}

> data NegotiationResult = NegotiationResult


% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

Agents' behavior is defined by \verb|AgentRoleDescriptor|s.

> groupRoleDescriptor :: AgentRoleDescriptor Group
> groupRoleDescriptor = AgentRoleDescriptor Group undefined -- TODO



> negotiatingAgentDescriptor :: RequitedData r ->
>     IO (GenericAgentDescriptor (AgentState r) NegotiationResult)
> negotiatingAgentDescriptor d = return GenericAgentDescriptor{
>   agName  = uniqueAgentName d,
>   agDebug = debugAgent d,
>   emptyResult = undefined :: EmptyResult NegotiationResult
> }



% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
