\documentclass{article}

%include format.fmt
%include polycode.fmt
%include forall.fmt

%format AnyFunc1 = "\mathrm{AnyFunc}_1"

\input{Document/Header}


%if False
\begin{code}

module Document () where

\end{code}
%endif

\begin{document}

\input{Document/Title}
\input{Document/Abstract}
\bigskip
\tableofcontents

%include Document/Implementation.lhs

%include GenericAgent.lhs

\end{document}


%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-copy-literate-src-flag: t
%%% eval: (load (concat (file-name-as-directory (projectile-project-root)) "publish-pdf.el"))
%%% End:


