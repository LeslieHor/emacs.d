(setq vc-follow-symlinks t) ;; To disable prompt on start up when using symlinks
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(setq safe-local-variable-values '((eval circ/file-local-eval-safe-auto-revert)))
(org-babel-load-file "~/.emacs.d/personalsettings.org")
(org-babel-load-file "~/.emacs.d/settings.org")
(org-babel-load-file "~/.emacs.d/configuration.org")
