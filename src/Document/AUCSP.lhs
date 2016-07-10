%if standalone
\documentclass{article}

%include format.fmt
%include polycode.fmt
%include forall.fmt

%format AnyFunc1 = "\mathrm{AnyFunc}_1"


\input{Header}

%endif


%if standalone
\begin{document}
%endif


\subsection{University Classes}
%include ../AUCSP/Classes.lhs

\subsection{Negotiating Agents}
%include ../AUCSP/NegotiationRoles.lhs

\subsection{Coherence}
%include ../AUCSP/Coherence.lhs

\subsection{Contexts}
%include ../AUCSP/Context.lhs

 
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
