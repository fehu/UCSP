
%if False
\begin{code}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module AUCSP.AgentsInterface (

  NegotiatorOfRole(..)
, NegotiatorOfRoleData(..)
, NegotiatorOfRoleRef(..)

, NegotiatorsWithRoles(..)
, NegotiatorsOfRole(..)


, SomeAgent(..), someAgent
, someAgentIsOfRole, someAgentIsOfRole', someAgentMatchRole

) where

  import Data.Typeable
  import Data.Function (on)

  import Control.Monad (when, (<=<), (>>))
  import Control.Arrow ( (&&&) )

\end{code}
%endif

Problem definition involves agents, but to remain independent from agents
implementation, only \verb|NegotiatorOfRole| interface is used to describe
necessary functionality.

Consists of:

\begin{enumerate}
  \item A container for the information below.

> class ( NegotiatorOfRoleData r, NegotiatorOfRoleRef r
>       , Typeable r, Show r, Eq r
>       , Show (KnownAgent r)) =>
>   NegotiatorOfRole r where
>
>   data KnownAgent r :: *
>   knownData :: KnownAgent r -> RoleData r
>   knownRef  :: KnownAgent r -> RoleRef r
>   roleOf   :: KnownAgent r -> r

>   knownAgentId :: KnownAgent r -> String

  \item Some data, associated with the role.

> class NegotiatorOfRoleData r where type RoleData r :: *

\item A reference to an agent of given role; depends on the used agents.

> class (Show (RoleRef r), Ord (RoleRef r)) =>
>   NegotiatorOfRoleRef r where type RoleRef r :: *

\end{enumerate}

> instance (NegotiatorOfRole r) => Eq (KnownAgent r) where (==) = (==) `on` knownRef

> instance (NegotiatorOfRole r) => Ord (KnownAgent r) where compare = compare `on` knownRef


It's often needed to pass a list of agent references without worrying about
handling their roles.

\begin{code}
  newtype NegotiatorsWithRoles = NegotiatorsWithRoles [NegotiatorsOfRole]
  data NegotiatorsOfRole = forall r . NegotiatorOfRole r => NegotiatorsOfRole r [KnownAgent r]
\end{code}





\verb|SomeAgent| hides role information, but allows it to be matched through
\verb|cast|.

> data SomeAgent = forall r . NegotiatorOfRole r =>
>      SomeAgent r (KnownAgent r)

> someAgent :: NegotiatorOfRole r => KnownAgent r -> SomeAgent
> someAgent = uncurry SomeAgent . (roleOf &&& id)

> someAgentMatchRole :: (NegotiatorOfRole r) =>
>                    r -> SomeAgent -> Maybe (KnownAgent r)
> someAgentMatchRole r (SomeAgent r' ar') = isSame r r' >> cast ar'
>     where isSame x = (`when` return ()) <=< fmap (x==) . cast

> someAgentIsOfRole :: (NegotiatorOfRole r) => SomeAgent -> r -> Bool
> someAgentIsOfRole (SomeAgent r' ar') = maybe False (r'==) . cast

> someAgentIsOfRole' :: (NegotiatorOfRole r) => SomeAgent -> Proxy r -> Bool
> someAgentIsOfRole' (SomeAgent r' ar') = maybe False (sameType r') . cast
>   where sameType :: a -> Proxy a -> Bool
>         sameType _ _ = True

> someAgentId (SomeAgent _ a) = knownAgentId a

> instance Eq  SomeAgent where (==)    = (==)    `on` someAgentId
> instance Ord SomeAgent where compare = compare `on` someAgentId

> instance Show SomeAgent where show (SomeAgent _ a) = show a
