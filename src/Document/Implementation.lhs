%if standalone
\documentclass{article}

%include format.fmt
%include polycode.fmt
%include forall.fmt

%format AnyFunc1 = "\mathrm{AnyFunc}_1"


\input{Header}

%endif

%if False
\begin{code}

module Document.Implementation where

-- import Data.Ix
import Data.Typeable
import Data.Either
import Data.Function (on)
import Data.IORef
import Data.Maybe
import Data.Coerce
import Data.Functor.Identity
import GHC.Int

import Data.Set (Set, union, member)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar

import GHC.Exts (groupWith)


import GenericAgent


\end{code}
%endif

%if standalone
\begin{document}
%endif

\section{Implementation}

%include Implementation/Classes.lhs
%include Implementation/NegotiationRoles.lhs
%include Implementation/Coherence.lhs
%include Implementation/Contexts.lhs




\subsection{Agent}

% Misc code:

%\begin{code}

% instance Show Class where -- TODO

% instance (Show a) => Show (InUnitInterval a) where show (InUnitInterval x) = show x

%\end{code}

%if standalone
\end{document}
%endif

 % eval: (when (not (haskell-session-maybe)) (haskell-session-change))

%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-copy-literate-src=flag: t
%%% lhs-build-standalone-flag: t
%%% eval: (haskell-indentation-mode)
%%% eval: (interactive-haskell-mode)
%%% eval: (load (concat (file-name-as-directory (projectile-project-root)) "publish-pdf.el"))
%%% End:
