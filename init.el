;; Disable activation of installed packages to force using manually updated
;; versions of built-in packages
(package-initialize nil)
(setq package-enable-at-startup nil)
(add-to-list 'load-path "~/.emacs.d/packages/eldoc-1.11.1")
(add-to-list 'load-path "~/.emacs.d/packages/flymake-1.2.2")
(add-to-list 'load-path "~/.emacs.d/packages/jsonrpc-1.0.15")
(add-to-list 'load-path "~/.emacs.d/packages/project-0.8.1")
(add-to-list 'load-path "~/.emacs.d/packages/seq-2.23")
(add-to-list 'load-path "~/.emacs.d/packages/xref-1.0.1")
(package-initialize t)
(setq vc-follow-symlinks t) ;; To disable prompt on start up when using symlinks
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(setq safe-local-variable-values '((eval circ/file-local-eval-safe-auto-revert)))
(org-babel-load-file "~/.emacs.d/personalsettings.org")
(org-babel-load-file "~/.emacs.d/settings.org")
(org-babel-load-file "~/.emacs.d/configuration.org")
(org-babel-load-file "~/.emacs.d/org-redefinitions.org")
