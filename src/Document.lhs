\documentclass{article}

%include Document/format.fmt
%include polycode.fmt
%include forall.fmt

%format AnyFunc1 = "\mathrm{AnyFunc}_1"

\input{Document/Header}

% \usepackage{showframe}


\begin{document}

\input{Document/Title}
\input{Document/Abstract}
\bigskip
\tableofcontents


\section{Implementation}
%include Document/AUCSP.lhs

\subsection{Generic Agent}
%include GenericAgent.lhs

\subsection{Agent Implementation}
%include AUCSP/NegotiatingAgent.lhs

\end{document}


%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-copy-literate-src-flag: t
%%% eval: (load (concat (file-name-as-directory (projectile-project-root)) "publish-pdf.el"))
%%% End:


