(org-babel-load-file "~/.emacs.d/configuration.org")
(put 'erase-buffer 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/projects/org/todo.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:height 1.0 :background "#222222"))))
 '(org-block-background ((t (:height 1.0 :background "#222222"))))
 '(org-block-begin-line ((t (:height 1.0 :foreground "#222222" :background "#070707"))) t)
 '(org-block-end-line ((t (:height 1.0 :foreground "#222222" :background "#070707"))) t)
 '(org-document-title ((t (:inherit default :weight bold :foreground "wheat" :height 2.0 :underline t))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :foreground "#e6194b"))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :foreground "#3cb44b"))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :foreground "#ffe119"))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :foreground "#4363d8"))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :foreground "#f58231"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :foreground "#911eb4"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :foreground "#42d4f4"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "wheat" :height 1.0 :foreground "#f032e6")))))
