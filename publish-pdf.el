(defun publish-pdf ()
  "Publish the pdf, corresponding to this 'lhs' file."
  (interactive)
  (when (y-or-n-p
         (concat "Publish " (file-name-base)
                 ".pdf on gh-pages?"))
    (shell-command
     (concat (file-name-as-directory (projectile-project-root))
             "publishPdf " (buffer-file-name) ".pdf"))
    )
  )
(local-set-key "\C-c\M-p" 'publish-pdf)
