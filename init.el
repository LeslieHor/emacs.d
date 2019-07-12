;;; Set up
(setq inhibit-startup-screen t) ; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore) ; silent bell when you make a mistake
(setq-default frame-title-format '("%b - Emacs")) ; set the emacs title.
                                        ; overrides the old "emacs@HOST" title
(setq-default indent-tabs-mode nil) ; don't insert tabs
(setq-default tab-width 4) ; self-documenting
(setq indent-line-function 'insert-tab)
(set-frame-font "DejaVu Sans Mono 8" nil t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(server-start) ; Start emacs server
(desktop-save-mode 1)
(load-theme 'wheatgrass)

;; Put the backup and autosave files away
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;;;; Whitespace
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;;;; org
(setq org-html-postamble "<p class=\"created\">Created: %T</p>")
; Set up languages for running code blocks in org
(org-babel-do-load-languages
 'org-babel-load-languages '(
                             (python . t)
                             (sh . t)
                             )
 )

;;;; Mode line customization
(column-number-mode 1) ; show column number
(set-face-attribute 'mode-line nil ; Set active mode line colour
                    :foreground "black"
                    :background "light blue")
(set-face-attribute 'mode-line-buffer-id nil ; Set buffer id colour
                    :foreground "white"
                    :background "dark green")

;;; custom functions
;; Taken from emacswiki.org
(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

;; Transpose windows
(defun transpose-windows (dir &optional arg)
  "Transpose the buffers in the current window and the target window
If arg is non-nil, the selected window will change to keep the source buffer
selected."
  (let ((target-window (windmove-find-other-window dir))
        (source-window (selected-window)))
    (cond ((null target-window)
           (format "No window found in dir %s" dir))
          ((and (window-minibuffer-p target-window)
                (not (minibuffer-window-active-p target-window)))
           (user-error "Minibuffer is inactive"))
          (t
           (let ((target-buffer (window-buffer target-window))
                 (source-buffer (window-buffer)))
             (set-window-buffer target-window source-buffer)
             (set-window-buffer source-window target-buffer))
           (if arg
               (select-window target-window))))))

(defun transpose-windows-left (&optional arg)
    "Transpose buffers from current window to buffer to the left"
  (interactive)
  (transpose-windows 'left arg))

(defun transpose-windows-up (&optional arg)
    "Transpose buffers from current window to buffer above"
  (interactive)
  (transpose-windows 'up arg))

(defun transpose-windows-right (&optional arg)
    "Transpose buffers from current window to buffer to the right"
  (interactive)
  (transpose-windows 'right arg))

(defun transpose-windows-down (&optional arg)
    "Transpose buffers from current window to buffer below"
  (interactive)
  (transpose-windows 'down arg))

;; Cast buffer
(defun cast-buffer (dir &optional arg)
  "Casts the current buffer to window in direction dir, and switches current
window back to last buffer.
If arg is non-nil, the targetted window is selected."
  (let ((target-window (windmove-find-other-window dir))
        (source-buffer (window-buffer)))
    (cond ((null target-window)
           (format "No window found in dir %s" dir))
          ((and (window-minibuffer-p target-window)
                (not (minibuffer-window-active-p target-window)))
           (user-error "Minibuffer is inactive"))
          (t
           (set-window-buffer target-window source-buffer)
           (previous-buffer)
           (if arg
               (select-window target-window))))))

(defun cast-buffer-left (&optional arg)
  "Cast current buffer to the left"
  (interactive)
  (cast-buffer 'left arg))

(defun cast-buffer-up (&optional arg)
  "Cast current buffer up"
  (interactive)
  (cast-buffer 'up arg))

(defun cast-buffer-right (&optional arg)
  "Cast current buffer to the right"
  (interactive)
  (cast-buffer 'right arg))

(defun cast-buffer-down (&optional arg)
  "Cast current buffer down"
  (interactive)
  (cast-buffer 'down arg))

;; Duplicate buffer

(defun duplicate-buffer (dir &optional arg)
  "Opens the current buffer in the window in the direction dir
If arg is non-nil, the targeted window is selected"
  (let ((target-window (windmove-find-other-window dir))
        (source-buffer (window-buffer)))
    (cond ((null target-window)
           (format "No window found in dir %s" dir))
          ((and (window-minibuffer-p target-window)
                (not (minibuffer-window-active-p target-window)))
           (user-error "Minibuffer is inactive"))
          (t
           (set-window-buffer target-window source-buffer)
           (if arg
               (select-window target-window))))))

(defun duplicate-buffer-left (&optional arg)
  "Cast current buffer to the left"
  (interactive)
  (duplicate-buffer 'left arg))

(defun duplicate-buffer-up (&optional arg)
  "Cast current buffer up"
  (interactive)
  (duplicate-buffer 'up arg))

(defun duplicate-buffer-right (&optional arg)
  "Cast current buffer to the right"
  (interactive)
  (duplicate-buffer 'right arg))

(defun duplicate-buffer-down (&optional arg)
  "Cast current buffer down"
  (interactive)
  (duplicate-buffer 'down arg))

;; Regenerate tags
(defun regenerate-tags ()
  ""
  (interactive)
  (let* ((root-dir (projectile-project-root))
         (find-command-file (concat root-dir ".emacs/find-command"))
         (find-search (get-string-from-file find-command-file))
         (find-command (concat "find " root-dir find-search))
         (tags-file (concat root-dir projectile-tags-file-name))
         (tag-command (concat "ctags -ef " tags-file " $(" find-command
                              ") && echo -n \"tags regenerated\""))
         (result (shell-command-to-string tag-command)))
    (user-error result)))

;;; which-key
(add-to-list 'load-path "~/.emacs.d/packages/which-key-3.3.1")
(require 'which-key)
(which-key-mode)

;;; evil
(add-to-list 'load-path "~/.emacs.d/packages/evil-1.2.14")
(require 'evil)
(evil-mode 1)
(evil-ex-define-cmd "q" 'kill-this-buffer) ; :q should kill the buffer rather
                                        ; than quiting emacs
(evil-ex-define-cmd "quit" 'evil-quit) ; :quit to quit emacs

;;; evil-leader
(add-to-list 'load-path "~/.emacs.d/packages/evil-leader-0.4.3")
(require 'evil-leader)
(global-evil-leader-mode)

;;; evil-org
(add-to-list
 'load-path
 "~/.emacs.d/packages/evil-org-mode-b6d652a9163d3430a9e0933a554bdbee5244bbf6")
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(add-hook 'org-mode-hook
          (lambda ()
          (setq evil-auto-indent nil)))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;;; evil-numbers
(add-to-list 'load-path "~/.emacs.d/packages/evil-numbers-0.4")
(require 'evil-numbers)

;;; evil-quickscope
(add-to-list 'load-path "~/.emacs.d/packages/evil-quickscope-0.1.4")
(require 'evil-quickscope)
(global-evil-quickscope-mode 1)

;;; ivy swiper counsel
(add-to-list 'load-path "~/.emacs.d/packages/swiper-0.11.0")
(require 'ivy)
(require 'swiper)
(require 'counsel)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;;; projectile
(add-to-list 'load-path "~/.emacs.d/packages/projectile-2.0.0")
(require 'projectile)
(projectile-mode +1)
(setq projectile-project-search-path '("~/projects/")) ; where the projects are
(setq projectile-completion-system 'ivy)

;;; counsel-projectile
(add-to-list 'load-path "~/.emacs.d/packages/counsel-projectile-0.3.0")
(require 'counsel-projectile)
(setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
                                        ; this required a fix that was taken
                                        ; from commit a07ddc8

;;; ranger
(add-to-list 'load-path "~/.emacs.d/packages/ranger.el-0.9.8.5")
(require 'ranger)

;;; neotree
(add-to-list 'load-path "~/.emacs.d/packages/emacs-neotree-0.5.2")
(require 'neotree)
(setq neo-theme 'arrow)
(setq neo-autorefresh nil)
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
(add-to-list 'load-path "~/.emacs.d/packages/rainbow-delimiters-2.1.3")
(require 'rainbow-delimiters)
(set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "#e6194B")
(set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#3cb44b")
(set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "#ffe119")
(set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "#4363d8")
(set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#f58231")
(set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "#911eb4")
(set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "#42d4f4")
(set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "#f032e6")
(set-face-attribute 'rainbow-delimiters-depth-9-face nil :foreground "#bfef45")

;;; beacon
(add-to-list 'load-path "~/.emacs.d/packages/beacon-1.3.4")
(require 'beacon)
(beacon-mode 1)

;;; json-mode
(add-to-list 'load-path "~/.emacs.d/packages/json-snatcher-1.0.0")
(add-to-list 'load-path "~/.emacs.d/packages/json-reformat-0.0.6")
(add-to-list 'load-path "~/.emacs.d/packages/json-mode-1.7.0")
(require 'json-mode)

;;; eyebrowse
(add-to-list 'load-path "~/.emacs.d/packages/dash.el-2.16.0")
(add-to-list 'load-path "~/.emacs.d/packages/eyebrowse-0.7.7")
(require 'eyebrowse)
(setq eyebrowse-new-workspace t) ; New workspaces start with scratch buffer
(eyebrowse-mode) ; enable global eyebrowse mode on start up

(setq eyebrowse-mode-line-left-delimiter "| ")
(setq eyebrowse-mode-line-right-delimiter " |")
(setq eyebrowse-mode-line-separator " | ")
(setq eyebrowse-mode-line-style 'always) ; Will show in title bar instead
(setq eyebrowse-tagged-slot-format "%s: %t")

;; Show workspaces in title bar
;; Only recalculate the workspaces string when it actually changes.
(defvar eyebrowse-workspaces)
(defun eyebrowse-workspaces-string ()
    "Get the current workspaces as a string."
    (let ((workspaces (substring-no-properties
                       (eyebrowse-mode-line-indicator))))
      (setq eyebrowse-workspaces
            (replace-regexp-in-string
             (format "| \\(%s.*?\\) |.*\\'" (eyebrowse--get 'current-slot))
             "> \\1 <"
             (substring-no-properties (eyebrowse-mode-line-indicator))
             nil nil 1))))
(defun eyebrowse-workspaces-string-rename (arg1 arg2)
    "Advice for `eyebrowse-rename-window-config'. Requires two
    arguments ARG1 and ARG2 to work..."
    (eyebrowse-workspaces-string))
(eyebrowse-workspaces-string)
(add-hook 'eyebrowse-post-window-switch-hook 'eyebrowse-workspaces-string)
(advice-add 'eyebrowse-close-window-config
            :after #'eyebrowse-workspaces-string)
(advice-add 'eyebrowse-rename-window-config
            :after #'eyebrowse-workspaces-string-rename)

;; Append to title list.
(add-to-list 'frame-title-format
            '(:eval (when (not (string-empty-p eyebrowse-workspaces))
                        (format "%s - " eyebrowse-workspaces))))

;;; telephone-line
(add-to-list 'load-path "~/.emacs.d/packages/telephone-line-0.4")
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
(add-to-list 'load-path "~/.emacs.d/packages/nlinum-1.8.1")
(require 'nlinum)

;;; nlinum-relative
(add-to-list
 'load-path
 "~/.emacs.d/packages/nlinum-relative-5b9950c97ba79a6f0683e38b13da23f39e01031c")
(require 'nlinum-relative)
(nlinum-relative-setup-evil)
(global-nlinum-relative-mode)
(setq nlinum-relative-redisplay-delay 0) ; delay
(setq nlinum-relative-current-symbol "") ; e.g. "->"
                                        ; "" for display current line number
(setq nlinum-relative-offset 0)          ; 1 if you want 0, 2, 3...

; Ensure relative mode remains on when in evil operator mode
(add-hook 'evil-operator-state-entry-hook
          (lambda () (when (bound-and-true-p nlinum-relative-mode)
                       (nlinum-relative-on))))
(add-hook 'evil-operator-state-exit-hook
          (lambda () (when (bound-and-true-p nlinum-relative-mode)
                       (nlinum-relative-off))))

;;; diff-hl
(add-to-list 'load-path "~/.emacs.d/packages/diff-hl-1.8.6")
(require 'diff-hl)
(require 'diff-hl-flydiff)
(global-diff-hl-mode)
(diff-hl-flydiff-mode)

;;; magit
(add-to-list
 'load-path
 "~/.emacs.d/packages/transient-01a166fcb8bbd9918ba741e9b5428a4b524eab33/lisp")
(add-to-list
 'load-path
 "~/.emacs.d/packages/hydra-0.15.0")
(add-to-list
 'load-path
 "~/.emacs.d/packages/with-editor-ff23166feb857e3cfee96cb1c9ef416a224a7e20")
(add-to-list
 'load-path
 "~/.emacs.d/packages/magit-23267cf33a7b690b27dc6760af8ab7f0886239ce/lisp")
(require 'magit)
(with-eval-after-load 'info
  (info-initialize)
  (add-to-list
   'Info-directory-list
   "~/.emacs.d/packages/magit-23267cf33a7b690b27dc6760af8ab7f0886239ce/\
Documentation/"))

;;; evil-magit
(add-to-list
 'load-path
 "~/.emacs.d/packages/evil-magit-ca83cfd246a9e808af3d42ee9bf740b81454fbd8")
(require 'evil-magit)

;;; erlang
(setq load-path (cons  "~/erl_rel/18.3/lib/tools-2.8.3/emacs" load-path))
(setq erlang-root-dir "~/erl_rel/18.3")
(setq exec-path (cons "~/erl_rel/18.3/bin" exec-path))
(require 'erlang-start)

;;; general
(add-to-list
 'load-path
 "~/.emacs.d/packages/general-2d2dd1d532fa75c1ed0c010d50e817ce43e58066/")
(require 'general)
(general-auto-unbind-keys)

;;;; p1 bindings
;;;;; Normal
(general-define-key
 "C-M-:" 'counsel-M-x
 "C-M-?" '((lambda() (interactive)(swiper (ivy-thing-at-point)))
           :which-key "swiper")
 "C-M-S-c" 'delete-window
 "C-M-S-h" 'windmove-left
 "C-M-S-d" 'evil-goto-definition
 "C-M-S-j" 'windmove-down
 "C-M-S-k" 'windmove-up
 "C-M-S-l" 'windmove-right
 "C-M-S-q" 'kill-buffer
 )
;;;;; Ranger
(general-define-key
 :keymaps 'ranger-mode-map
 "C-M-S-c" 'ranger-disable
 )

;;;;; Org mode
(general-define-key
 :keymaps 'org-mode-map
 "C-M-S-o" 'org-open-at-point
 )

;;;;; Erlang mode
(general-define-key
 :keymaps 'erlang-mode-map
 "C-M-S-d" '((lambda () (interactive)(erlang-find-tag (erlang-find-tag-default)))
             :which-key "goto erlang definition") ; For some reason, this
                                        ; property is required this keybinding
                                        ; to work
 )

;;;; File / directory related
(general-create-definer files-leader
 :prefix "C-M-S-f")
;;;;; Normal
(files-leader
 "f" '(counsel-find-file :which-key "find file") ; find file using ivy
 "r" '(counsel-recentf :which-key "find recent file") ; find recently edited
                                        ; files
 "b" '(ivy-switch-buffer :which-key "buffers")
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
 "e" 'balance-windows
 "r" '(revert-buffer :which-key "reload from disk")
 "t" '(:ignore t :which-key "transpose windows")
 "th" '((lambda () (interactive)(transpose-windows-left t)) ; weird syntax is
        :which-key "transpose windows left")                ; for calling the
 "tj" '((lambda () (interactive)(transpose-windows-down t)) ; function with
        :which-key "transpose windows down")                ; arguments
 "tk" '((lambda () (interactive)(transpose-windows-up t))
        :which-key "transpose windows up")
 "tl" '((lambda () (interactive)(transpose-windows-right t))
        :which-key "transpose windows right")
 "c" '(:ignore t :which-key "cast buffer")
 "ch" '((lambda () (interactive)(cast-buffer-left t))
        :which-key "cast buffer left")
 "cj" '((lambda () (interactive)(cast-bufer-down t))
        :which-key "cast buffer down")
 "ck" '((lambda () (interactive)(cast-buffer-up t))
        :which-key "cast buffer up")
 "cl" '((lambda () (interactive)(cast-buffer-right t))
        :which-key "cast buffer right")
 )
;;;;; Neotree
(windows-leader
 :keymaps 'neotree-mode-map
 "z" '(neotree-stretch-toggle :which-key "maximize tree pane")
 )

;;;; Buffer related
(general-create-definer buffer-leader
  :prefix "C-M-S-b")
;;;;; Normal
(buffer-leader
 "b" '(ivy-switch-buffer :which-key "switch buffers")
 "d" '(:ignore t :which-key "duplicate buffer")
 "dh" '((lambda () (interactive)(duplicate-buffer-left t))
        :which-key "duplicate buffer left")
 "dj" '((lambda () (interactive)(cast-bufer-down t))
        :which-key "duplicate buffer down")
 "dk" '((lambda () (interactive)(duplicate-buffer-up t))
        :which-key "duplicate buffer up")
 "dl" '((lambda () (interactive)(duplicate-buffer-right t))
        :which-key "duplicate buffer right")
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
  "tr" '(org-table-toggle-coordinate-overlays
         :which-key "toggle table reference")
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
 "tc" '(org-table-create-or-convert-from-region
        :which-key "create / convert table")
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
(edits-leader
  :keymaps 'erlang-mode-map
  "a" '(erlang-align-arrows :which-key "align arrows")
  "i" '(erlang-indent-region :which-key "indent region")
  )

;;;; Registers
(general-create-definer registers-leader
  :prefix "C-M-S-r")
(registers-leader
  "p" '(:ignore t :which-key "positions")
  "py" '(point-to-register :which-key "yank current position to register")
  "pp" '(jump-to-register :which-key "goto register position")
  "t" '(:ignore t :which-key "text")
  "ty" '(copy-to-register :which-key "yank text to register")
  "tp" '(insert-register :which-key "paste text register")
  "ta" '(append-to-register :which-key "append text to register")
  "tb" '(prepend-to-register :which-key "prepend text to register (before)")
  "r" '(:ignore t :which-key "rectangle")
  "ry" '(copy-rectangle-to-register :which-key "yank rectangle to register")
  "rp" '(insert-register :which-key "paste rectangle from register")
  "n" '(:ignore t :which-key "numbers")
  "ny" '(number-to-register :which-key "yank number to register")
  "np" '(insert-register :which-key "paste number from register")
  "ni" '(increment-register :which-key "increment register with number")
 )

;;;; Applications
(general-create-definer applications-leader
  :prefix "C-M-S-o")
(applications-leader
 "r" '(ranger :which-key "ranger")
 "s" '(eshell-new :which-key "eshell")
 "e" '(erlang-shell :which-key "erlang shell")
 )

;;;; Version control
(general-create-definer version-control-leader
  :prefix "C-M-S-v")
(version-control-leader
 "s" '(magit-status :which-key "status")
 "b" '(magit-blame :which-key "blame")
 "d" '(magit-diff :which-key "diff")
 "m" '(magit-merge :which-key "merge")
 "b" '(magit-branch :which-key "branch")
 )

;;;; prog2 bindings
(general-define-key
 :states '(normal insert visual operator) ; This is just for demonstration
                                        ; purposes. Just to remind me how to do
                                        ; this.
 :keymaps 'override ; required to override evil-org's C-S-hjkl mappings
 "C-S-h" 'shrink-window-horizontally
 "C-S-j" 'enlarge-window
 "C-S-k" 'shrink-window
 "C-S-l" 'enlarge-window-horizontally
 )

;;;; eyebrowse bindings
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
 "C-M-<" 'eyebrowse-prev-window-config
 "C-M->" 'eyebrowse-next-window-config
 "C-M-w" 'eyebrowse-last-window-config
 "C-M-h" 'evil-jump-backward
 "C-M-l" 'evil-jump-forward
 )

(general-create-definer frames-leader
  :prefix "C-M-f")
(frames-leader
 "r" '(eyebrowse-rename-window-config :which-key "rename frame")
 "q" '(eyebrowse-close-window-config :which-key "close frame")
 "c" '(eyebrowse-create-window-config :which-key "create frame")
 )

(general-create-definer projects-leader
  :prefix "C-M-S-p")
(projects-leader
 "?" '(projectile-command-map :which-key "other")
 "p" '(counsel-projectile-switch-project :which-key "switch project")
 "g" '(counsel-projectile-grep :which-key "find instances in project")
 "f" '(counsel-projectile-find-file :which-key "find project file") ; find file
                                        ; in current project
 "s" '(org-store-link :which-key "copy link")
 "i" '(org-insert-link :which-key "insert link")
 "t" '(regenerate-tags :which-key "regenerate tags")
 )

;;; Help overrides
(general-define-key
 "C-h v" 'counsel-describe-variable
 "C-h f" 'counsel-describe-function
 )
