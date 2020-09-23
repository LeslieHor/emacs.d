(setq vc-follow-symlinks t) ;; To disable prompt on start up when using symlinks
(put 'narrow-to-region 'disabled nil)
(setq safe-local-variable-values '((eval circ/file-local-eval-safe-auto-revert)))
(org-babel-load-file "~/.emacs.d/personalsettings.org")
(org-babel-load-file "~/.emacs.d/settings.org")
(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "ddrr" :query "to:leslie.hor@gmail.com path:\"gmail/Dream Journal/**\"")
     (:name "Gmail Inbox" :query "path:gmail/** tag:inbox")
     (:name "Mailbox Inbox" :query "path:mailbox/** tag:inbox")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:height 1.0 :background "#222222"))))
 '(org-block-background ((t (:height 1.0 :background "#222222"))))
 '(org-block-begin-line ((t (:height 1.0 :foreground "#222222" :background "#070707"))))
 '(org-block-end-line ((t (:height 1.0 :foreground "#222222" :background "#070707"))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :underline t))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :foreground "#e6194b"))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :foreground "#3cb44b"))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :foreground "#ffe119"))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :foreground "#4363d8"))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :foreground "#f58231"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :foreground "#911eb4"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :foreground "#42d4f4"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :foreground "#f032e6")))))
