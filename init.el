;;; Set up
(setq inhibit-startup-screen t) ; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore) ; silent bell when you make a mistake
(setq-default frame-title-format '("%b [%m] - emacs")) ; set the emacs title. overrides the old "emacs@HOST" title
(setq-default indent-tabs-mode nil) ; don't insert tabs
(setq-default tab-width 4) ; self-documenting
(setq indent-line-function 'insert-tab)
(set-frame-font "DejaVu Sans Mono 8" nil t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(server-start) ; Start emacs server

;;;; Mode line customization
(column-number-mode 1) ; show column number
(set-face-attribute 'mode-line nil :foreground "black" :background "light blue") ; Set active mode line colour
(set-face-attribute 'mode-line-buffer-id nil :foreground "white" :background "dark green") ; Set buffer id colour

;;; which-key
(add-to-list 'load-path "~/.emacs.d/packages/which-key-3.3.1/")
(require 'which-key)
(which-key-mode)

;;; evil
(add-to-list 'load-path "~/.emacs.d/packages/evil-1.2.14/")
(require 'evil)
(evil-mode 1)
(evil-ex-define-cmd "q" 'kill-this-buffer) ; :q should kill the buffer rather than quiting emacs
(evil-ex-define-cmd "quit" 'evil-quit) ; :quit to quit emacs

;;; evil-leader
(add-to-list 'load-path "~/.emacs.d/packages/evil-leader-0.4.3/")
(require 'evil-leader)
(global-evil-leader-mode)

;;; evil-org
(add-to-list 'load-path "~/.emacs.d/packages/evil-org-mode-b6d652a9163d3430a9e0933a554bdbee5244bbf6/")
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(add-hook 'org-mode-hook
          (lambda ()
          (setq evil-auto-indent nil)))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;;; evil-numbers
(add-to-list 'load-path "~/.emacs.d/packages/evil-numbers-0.4/")
(require 'evil-numbers)

;;; evil-quickscope
(add-to-list 'load-path "~/.emacs.d/packages/evil-quickscope-0.1.4/")
(require 'evil-quickscope)
(global-evil-quickscope-mode 1)

;;; ivy swiper counsel
(add-to-list 'load-path "~/.emacs.d/packages/swiper-0.11.0/")
(require 'ivy)
(require 'swiper)
(require 'counsel)
(setq ivy-use-virtual-buffers t)

;;; projectile
(add-to-list 'load-path "~/.emacs.d/packages/projectile-2.0.0/")
(require 'projectile)
(projectile-mode +1)
(setq projectile-project-search-path '("~/projects/")) ; where the projects are
(setq projectile-completion-system 'ivy)

;;; counsel-projectile
(add-to-list 'load-path "~/.emacs.d/packages/counsel-projectile-0.3.0/")
(require 'counsel-projectile)

;;; ranger
(add-to-list 'load-path "~/.emacs.d/packages/ranger.el-0.9.8.5/")
(require 'ranger)

;;; neotree
(add-to-list 'load-path "~/.emacs.d/packages/emacs-neotree-0.5.2/")
(require 'neotree)
(setq neo-theme 'arrow)
; Neotree keybindings conflict with evil-mode
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

;;; rainbow-delimiters
(add-to-list 'load-path "~/.emacs.d/packages/rainbow-delimiters-2.1.3/")
(require 'rainbow-delimiters)

;;; beacon
(add-to-list 'load-path "~/.emacs.d/packages/beacon-1.3.4/")
(require 'beacon)
(beacon-mode 1)

;;; json-mode
(add-to-list 'load-path "~/.emacs.d/packages/json-snatcher-1.0.0/")
(add-to-list 'load-path "~/.emacs.d/packages/json-reformat-0.0.6/")
(add-to-list 'load-path "~/.emacs.d/packages/json-mode-1.7.0/")
(require 'json-mode)

;;; eyebrowse
(add-to-list 'load-path "~/.emacs.d/packages/dash.el-2.16.0/")
(add-to-list 'load-path "~/.emacs.d/packages/eyebrowse-0.7.7/")
(require 'eyebrowse)
(setq eyebrowse-new-workspace t) ; New workspaces start with scratch buffer
(eyebrowse-mode) ; enable global eyebrowse mode on start up

;;; telephone-line
(add-to-list 'load-path "~/.emacs.d/packages/telephone-line-0.4/")
(require 'telephone-line)
(setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
        (accent . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
        (nil    . (telephone-line-buffer-segment))))
(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-major-mode-segment))
        (evil   . (telephone-line-airline-position-segment))))
(telephone-line-mode 1)

;;; nlinum
(add-to-list 'load-path "~/.emacs.d/packages/nlinum-1.8.1/")
(require 'nlinum)

;;; nlinum-relative
(add-to-list 'load-path "~/.emacs.d/packages/nlinum-relative-5b9950c97ba79a6f0683e38b13da23f39e01031c/")
(require 'nlinum-relative)
(nlinum-relative-setup-evil)
(global-nlinum-relative-mode)
(setq nlinum-relative-redisplay-delay 0) ;; delay
(setq nlinum-relative-current-symbol "") ;; "->". or "" for display current line number
(setq nlinum-relative-offset 0)          ;; 1 if you want 0, 2, 3...

; Ensure relative mode remains on when in evil operator mode
(add-hook 'evil-operator-state-entry-hook
          (lambda () (when (bound-and-true-p nlinum-relative-mode) (nlinum-relative-on))))
(add-hook 'evil-operator-state-exit-hook
          (lambda () (when (bound-and-true-p nlinum-relative-mode) (nlinum-relative-off))))

;;; diff-hl
(add-to-list 'load-path "~/.emacs.d/packages/diff-hl-1.8.6/")
(require 'diff-hl)
(require 'diff-hl-flydiff)
(global-diff-hl-mode)
(diff-hl-flydiff-mode)

;;; general
(add-to-list 'load-path "~/.emacs.d/packages/general-2d2dd1d532fa75c1ed0c010d50e817ce43e58066/")
(require 'general)

(general-auto-unbind-keys)
;;;; p1 bindings
;;;;; Normal
(general-define-key
 "C-M-S-q" 'kill-buffer
 "C-M-S-c" 'delete-window
 "C-M-S-v" 'magit-status
 "C-M-S-b" 'ivy-switch-buffer
 "C-M-S-p" 'projectile-command-map
 "C-M-S-h" 'windmove-left
 "C-M-S-j" 'windmove-down
 "C-M-S-k" 'windmove-up
 "C-M-S-l" 'windmove-right
 "C-M-:" 'counsel-M-x
 "C-M-?" 'swiper
 )
;;;;; Ranger
(general-define-key
 :keymaps 'ranger-mode-map
 "C-M-S-c" 'ranger-disable
 )

;;;;; Org mode
(general-define-key
 :keymaps 'org-mode-map
 "C-M-S-g" 'org-open-at-point
 )

;;;; File / directory related
(general-create-definer files-leader
 :prefix "C-M-S-f")
;;;;; Normal
(files-leader
 "f" '(counsel-find-file :which-key "find file") ; find file using ivy
 "r" '(counsel-recentf :which-key "find recent file") ; find recently edited files
 "b" '(ivy-switch-buffer :which-key "buffers")
 "p" '(counsel-projectile-find-file :which-key "find project file") ; find file in current project
 "e" 'ranger ; explorer
 )
;;;;; Org-mode
(files-leader
 :keymaps 'org-mode-map
 "h" '(org-html-export-to-html :which-key "export to html file")
 )
;;;;; Ranger
(files-leader
 :keymaps 'ranger-mode-map
 "d" '(dired-create-directory :which-key "create directory")
 )
;;;;; Neotree
(files-leader
 :keymaps 'neotree-mode-map
 "n" '(neotree-create-node :which-key "create directory / file")
 "d" '(neotree-delete-node :which-key "delete directory / file")
 "r" '(neotree-rename-node :which-key "rename directory / file")
 "t" '(neotree-change-root :which-key "change root directory")
 "c" '(neotree-copy-node :which-key "Copy directory / file")
 )

;;;; Motion related
(general-create-definer motions-leader
 :prefix "C-M-S-m")
;;;;; org-mode
(motions-leader
 :keymaps 'org-mode-map
 "k" '(outline-previous-visible-heading :which-key "prev heading")
 "j" '(outline-next-visible-heading :which-key "next heading")
 "h" '(outline-backward-same-level :which-key "prev same level heading")
 "l" '(outline-forward-same-level :which-key "next same level heading")
 "g" '(outline-up-heading :which-key "move up a heading")
 )

;;;; Window related
(general-create-definer windows-leader
 :prefix "C-M-S-w")
;;;;; Normal
(windows-leader
 "v" 'split-window-below
 "h" 'split-window-right
 "z" 'maximize-window
 "o" 'delete-other-windows
 )
;;;;; Neotree
(windows-leader
 :keymaps 'neotree-mode-map
 "z" '(neotree-stretch-toggle :which-key "maximize tree pane")
 )

;;;; Toggles
(general-create-definer toggles-leader
  :prefix "C-M-S-t")
;;;;; Normal
(toggles-leader
 "w" '(whitespace-mode :which-key "toggle whitespace display")
 "n" '(neotree-toggle :which-key "toggle neotree")
 "p" '(projectile-mode :which-key "toggle projectile mode")
 "f" '(font-lock-mode :which-key "toggle auto font formatting")
 "c" '(comment-line :which-key "comment line")
 "r" '(rainbow-delimiters-mode :which-key "rainbow delimiters")
 "l" '(nlinum-mode :which-key "toggle relative line numbers")
 )
;;;;; org-mode
(toggles-leader
  :keymaps 'org-mode-map
  "x" '(org-toggle-checkbox :which-key "toggle checkbox")
  "i" '(org-toggle-inline-images :which-key "toggle images")
  "t" '(:ignore t :which-key "todos / tables")
  "ts" '(org-todo :which-key "cycle todo status")
  "tr" '(org-table-toggle-coordinate-overlays :which-key "toggle table reference")
  )
;;;;; Ranger
(toggles-leader
  :keymaps 'ranger-mode-map
  "h" '(ranger-toggle-dotfiles :which-key "toggle hidden")
  )
;;;;; Neotree
(toggles-leader
  :keymaps 'neotree-mode-map
  "c" '(neotree-collapse-all :which-key "collapse all")
  "h" '(neotree-hidden-file-toggle :which-key "toggle hidden")
  )

;;;; Specialized edits
(general-create-definer edits-leader
  :prefix "C-M-S-e")
;;;;; org-mode
(edits-leader
 :keymaps 'org-mode-map
 "t" '(:ignore t :which-key "tables")
 "tc" '(org-table-create-or-convert-from-region :which-key "create / convert table")
 "td" '(:ignore t :which-key "delete")
 "tdc" '(org-table-delete-column :which-key "delete column")
 "tdr" '(org-table-kill-row :whick-key "delete row")
 "ti" '(:ignore t :which-key "insert")
 "tic" '(org-table-insert-column :which-key "insert column")
 "tir" '(org-table-insert-row :which-key "insert row")
 "tih" '(org-table-insert-hline :which-key "insert horizontal line")
 "l" '(org-insert-link :which-key "edit link")
 "s" '(org-sort :which-key "sort")
 )

;;;; prog2 bindings
(general-define-key
 "C-S-h" 'shrink-window-horizontally
 "C-S-j" 'enlarge-window
 "C-S-k" 'shrink-window
 "C-S-l" 'enlarge-window-horizontally
 )

;;;; prog3 bindings
(general-define-key
 "C-M-1" 'eyebrowse-switch-to-window-config-1
 "C-M-2" 'eyebrowse-switch-to-window-config-2
 "C-M-3" 'eyebrowse-switch-to-window-config-3
 "C-M-4" 'eyebrowse-switch-to-window-config-4
 "C-M-5" 'eyebrowse-switch-to-window-config-5
 "C-M-6" 'eyebrowse-switch-to-window-config-6
 "C-M-7" 'eyebrowse-switch-to-window-config-7
 "C-M-8" 'eyebrowse-switch-to-window-config-8
 "C-M-9" 'eyebrowse-switch-to-window-config-9
 "C-M-," 'eyebrowse-prev-window-config
 "C-M-." 'eyebrowse-next-window-config
 "C-M-w" 'eyebrowse-last-window-config
 "C-M-h" 'back-button-global-backward
 "C-M-j" 'back-button-local-backward
 "C-M-k" 'back-button-local-forward
 "C-M-l" 'back-button-global-forward
 )

(general-create-definer frames-leader
  :prefix "C-M-f")
(frames-leader
 "r" '(eyebrowse-rename-window-config :which-key "rename frame")
 "q" '(eyebrowse-close-window-config :which-key "close frame")
 "c" '(eyebrowse-create-window-config :which-key "create frame")
 )
