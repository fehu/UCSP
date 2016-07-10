
%if False
\begin{code}

module Document.Implementation.Coherence(
  module Document.Implementation.Coherence.Information
, module Document.Implementation.Coherence.Relations

) where

import Document.Implementation.Coherence.Information
import Document.Implementation.Coherence.Relations

\end{code}
%endif

\subsection{Coherence}

The coherence mechanism is based on~\cite{Sindhu2010}.
It uses the \emph{contexts} as means of separating (and further prioritizing)
different \emph{cognitive aspects}. The contexts used are based on \emph{BDI}
agent architecture.

The \emph{combined coherence} is used as a measure of goal achievement.
It's combined of coherence values, calculated by agent's contexts.

\medskip\noindent
The \emph{binary relations} connect some information pieces, assigning to
the edge some value. The \emph{whole graph relations}, on the other side,
are applied to the graph as a whole and produce a single value.

The relations used, as well as the information in the graph,
depend on the \emph{context}.

The coherence is calculated over an \emph{information graph}, that represents
some aspect of agent's knowledge. The nodes of the graph are some
\emph{pieces of information} and the edges represent some \emph{relations}
between theese pieces.


%include Coherence/Information.lhs
%include Coherence/Relations.lhs



