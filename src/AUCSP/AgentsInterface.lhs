
%if False
\begin{code}

module AUCSP.AgentsInterface (

  AgentOfRole(..)
, AgentOfRoleData(..)
, AgentOfRoleRef(..)

, AgentsWithRoles(..)
, AgentsOfRole(..)


, SomeAgent(..), someAgent, someAgentMatchRole

) where

  import Data.Typeable
  import Data.Function (on)

  import Control.Monad (when, (<=<), (>>))

\end{code}
%endif

Problem definition involves agents, but to remain independent from agents
implementation, only \verb|AgentOfRole| interface is used to describe
necessary functionality.

Consists of:

\begin{enumerate}
  \item A container for the information below.

> class ( AgentOfRoleData r, AgentOfRoleRef r
>       , Typeable r, Show r, Eq r
>       , Show (KnownAgent r)) =>
>   AgentOfRole r where
>
>   roleInstance :: r
>
>   data KnownAgent r :: *
>   roleData :: KnownAgent r -> RoleData r
>   roleRef  :: KnownAgent r -> RoleRef r

>   roleAgentId :: KnownAgent r -> String

  \item Some data, associated with the role.

> class AgentOfRoleData r where type RoleData r :: *

\item A reference to an agent of given role; depends on the used agents.

> class (Show (RoleRef r), Ord (RoleRef r)) =>
>   AgentOfRoleRef r where type RoleRef r :: *

\end{enumerate}

> instance (AgentOfRole r) => Eq (KnownAgent r) where (==) = (==) `on` roleRef

> instance (AgentOfRole r) => Ord (KnownAgent r) where compare = compare `on` roleRef


It's often needed to pass a list of agent references without worrying about
handling their roles.

\begin{code}
  newtype AgentsWithRoles = AgentsWithRoles [AgentsOfRole]
  data AgentsOfRole = forall r . AgentOfRole r => AgentsOfRole r [KnownAgent r]
\end{code}





\verb|SomeAgent| hides role information, but allows it to be matched through
\verb|cast|.

> data SomeAgent = forall r . AgentOfRole r =>
>      SomeAgent r (KnownAgent r)

> someAgent :: AgentOfRole r => KnownAgent r -> SomeAgent
> someAgent = SomeAgent roleInstance

> someAgentMatchRole :: (AgentOfRole r) =>
>                    r -> SomeAgent -> Maybe (KnownAgent r)
> someAgentMatchRole r (SomeAgent r' ar') = isSame r r' >> cast ar'
>     where isSame x = (`when` return ()) <=< fmap (x==) . cast

> someAgentId (SomeAgent _ a) = roleAgentId a

> instance Eq  SomeAgent where (==)    = (==)    `on` someAgentId
> instance Ord SomeAgent where compare = compare `on` someAgentId

> instance Show SomeAgent where show (SomeAgent _ a) = show a
