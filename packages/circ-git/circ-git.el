;;; circ-git.el --- Helpful git functions
;; Under construction.

(defcustom circ-git-mode-hook nil
  "circ-git hook"
  :group 'circ-git
  :type 'hook)

(defvar circ-git-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `circ-git-mode'.")

(define-derived-mode circ-git-mode special-mode "circ-git"
  "Major mode for circ-git."
  (set (make-local-variable 'revert-buffer-function) #'circ-git))

;; Vars

(defvar circ-git-status-buffer-name "*circ-git-status*"
  "Name for the circ-git status buffer")

(defvar-local circ-git-buffer-info '())

;; Faces

(defface circ-git-staged-face
  '((t :foreground "#44FF44"))
  "Face for staged files"
  :group 'circ-git)

(defface circ-git-unstaged-face
  '((t :foreground "#FF4444"))
  "Face for unstaged files"
  :group 'circ-git)

(defface circ-git-untracked-face
  '((t :foreground "#FF4444"))
  "Face for untracked files"
  :group 'circ-git)

;; Evil bindings
(defun evil-circ-git-set-keys ()
  (evil-set-initial-state 'circ-git-mode 'motion)
  (evil-define-key 'motion circ-git-mode-map
    "q" 'circ/kill-this-buffer
    "r" 'circ-git

    "o" 'circ-git-open-file
    "s" 'circ-git-stage-file
    "u" 'circ-git-unstage-file
    "d" 'circ-git-vc-diff

    "n" 'circ-git-move-to-next-file
    "p" 'circ-git-move-to-prev-file
    ))
(evil-circ-git-set-keys)

(defun circ-git (&optional ignore-auto noconfirm)
  (interactive)
  (let ((buf (get-buffer-create circ-git-status-buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (switch-to-buffer buf)
      (circ-git-build-status-info)
      (dolist (line circ-git-buffer-info)
        (insert (cdr line) "\n"))
      (circ-git-mode)
      (circ-git-build-status-info) ;; TODO FIX: Have to run twice to get the info in variable
      (run-hooks 'circ-git-mode-hook))))

(defun circ-git-call-git (&optional dir destination &rest args)
  "Call git with args"
  (let ((default-directory (or dir (projectile-ensure-project (projectile-project-root)))))
    (if (not (listp args))
        (setq args (list args)))
    (apply 'call-process "git" nil destination nil args)))

(defun circ-git-get-file-change-states (&optional dir)
  (with-temp-buffer
    (circ-git-call-git dir t "status" "--porcelain")
    (mapcar
     (lambda (line)
       (when (not (string= "" line))
         (message "DEBUG: %s" line)
         (let ((state (substring line 0 2))
               (file (substring line 3 nil)))
           (cons
            (cond ((string= "M " state) 'modified-staged)
                  ((string= "A " state) 'new-staged)
                  ((string= " M" state) 'modified-unstaged)
                  ((string= "??" state) 'untracked)
                  (t 'unknown))
            file))))
     (split-string (string-trim-right (buffer-string)) "\n"))))

(defun circ-git-get-current-branch (&optional dir)
  (with-temp-buffer
    (circ-git-call-git dir t "rev-parse" "--abbrev-ref" "HEAD")
    (string-trim (buffer-string))))

(defun circ-git-get-remote-branch (&optional dir)
  (with-temp-buffer
    (circ-git-call-git dir t "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{u}")
    (string-trim (buffer-string))))

(defun circ-git-get-ahead-behind (&optional dir)
  (-
   ;; Ahead
   (with-temp-buffer
     (circ-git-call-git dir t "rev-list" "--count" "origin/HEAD..HEAD")
     (string-to-number (string-trim (buffer-string))))
   ;; Behind
   (with-temp-buffer
     (circ-git-call-git dir t "rev-list" "--count" "HEAD..origin/HEAD")
     (string-to-number (string-trim (buffer-string))))))

;; Actions

(defun circ-git-get-current-line ()
  (interactive)
  (nth (- (line-number-at-pos) 1) circ-git-buffer-info))

(defun circ-git-stage-file ()
  (interactive)
  (let ((line-alist (car (circ-git-get-current-line)))
        (line-pos (line-number-at-pos)))
    (when (and (eq 'file (cdr (assoc 'type line-alist)))
               (or (eq 'modified-unstaged (cdr (assoc 'state line-alist)))
                   (eq 'untracked (cdr (assoc 'state line-alist)))))
      (circ-git-call-git nil nil "add" (cdr (assoc 'filepath line-alist)))
      (message "Staged: %s" (cdr (assoc 'filepath line-alist)))
      (circ-git)
      (goto-char (point-min))
      (move-beginning-of-line line-pos))))

(defun circ-git-unstage-file ()
  (interactive)
  (let ((line-alist (car (circ-git-get-current-line)))
        (line-pos (line-number-at-pos)))
    (when (and (eq 'file (cdr (assoc 'type line-alist)))
               (or (eq 'modified-staged (cdr (assoc 'state line-alist)))
                   (eq 'new-staged (cdr (assoc 'state line-alist)))))
      (circ-git-call-git nil nil "restore" "--staged" (cdr (assoc 'filepath line-alist)))
      (message "Unstaged: %s" (cdr (assoc 'filepath line-alist)))
      (circ-git)
      (goto-char (point-min))
      (move-beginning-of-line line-pos))))

(defun circ-git-open-file ()
  (interactive)
  (let ((line-alist (car (circ-git-get-current-line))))
    (when (eq 'file (cdr (assoc 'type line-alist)))
      (find-file (cdr (assoc 'filepath line-alist))))))
  
(defun circ-git-move-to-next-file ()
  (interactive)
  (let ((buffer-info-cdr (subseq circ-git-buffer-info
                                 (line-number-at-pos) ))
        (counter 1))
    (dolist (line buffer-info-cdr)
      (setq counter (+ counter 1))
      (when (eq 'file (cdr (assoc 'type (car line))))
        (move-beginning-of-line counter)
        (return)))))
        
(defun circ-git-move-to-prev-file ()
  (interactive)
  (let ((buffer-info-car (reverse (subseq circ-git-buffer-info
                                          0 (- (line-number-at-pos) 1))))
        (counter 1))
    (dolist (line buffer-info-car)
      (setq counter (- counter 1))
      (when (eq 'file (cdr (assoc 'type (car line))))
        (move-beginning-of-line counter)
        (return)))))

(defun circ-git-vc-diff ()
  (interactive)
  (let ((line-alist (car (circ-git-get-current-line)))
        (window-width (window-width))
        (window-height (window-height)))
    (when (eq 'file (cdr (assoc 'type line-alist)))
      (vc-diff-internal nil
                        (list 'Git
                              (list (cdr (assoc 'filepath line-alist))))
                        nil nil t)
      ;; Reset window size to fix the custom window resizing of vc-diff
      (window-resize nil (- window-width (window-width)) t)
      (window-resize nil (- window-height (window-height))))))
  
(defun circ-git-build-status-info (&optional dir)
  (setq circ-git-buffer-info
        (let* ((branch (circ-git-get-current-branch dir))
               (file-states (circ-git-get-file-change-states dir))
               (modified-staged-files (seq-filter (lambda (f)
                                                    (eq 'modified-staged (car f)))
                                                  file-states))
               (new-staged-files (seq-filter (lambda (f)
                                               (eq 'new-staged (car f)))
                                             file-states))
               (modified-unstaged-files (seq-filter (lambda (f)
                                                      (eq 'modified-unstaged (car f)))
                                                    file-states))
               (untracked-files (seq-filter (lambda (f)
                                              (eq 'untracked (car f)))
                                            file-states))
               (remote-branch (circ-git-get-remote-branch dir))
               (ahead-behind (circ-git-get-ahead-behind dir))
               )
          (append
           ;; Branch
           (list
            (cons (list (cons 'type 'branch) (cons 'branch branch)) (format "Branch: %s" branch))

            ;; Ahead / Behind
            (cons (list (cons 'type 'ahead-behind) (cons 'ahead-behind ahead-behind)
                        (cons 'remote-branch remote-branch))
                  (cond ((= 0 ahead-behind)
                         (format "Branch is up to date with %s" remote-branch))
                        ((> 0 ahead-behind)
                         (format "Branch is behind of %s by %s commits" remote-branch ahead-behind))
                        ((< 0 ahead-behind)
                         (format "Branch is ahead of %s by %s commits" remote-branch ahead-behind)))))

           ;; Modified Staged files
           (list
            (cons nil "")
            (cons nil "Modified Staged:"))
           
           (mapcar 
            (lambda (file)
              (cons (list (cons 'type 'file) (cons 'state (car file))
                          (cons 'filepath (cdr file)))
                    (format "  %s" (propertize (cdr file) 'face 'circ-git-staged-face))))
            modified-staged-files)

           ;; New Staged files
           (list
            (cons nil "")
            (cons nil "New Files Staged:"))
           
           (mapcar 
            (lambda (file)
              (cons (list (cons 'type 'file) (cons 'state (car file))
                          (cons 'filepath (cdr file)))
                    (format "  %s" (propertize (cdr file) 'face 'circ-git-staged-face))))
            new-staged-files)

           ;; Modified Unstaged files
           (list
            (cons nil "")
            (cons nil "Unstaged:"))

           (mapcar 
            (lambda (file)
              (cons (list (cons 'type 'file) (cons 'state (car file))
                          (cons 'filepath (cdr file)))
                    (format "  %s" (propertize (cdr file) 'face 'circ-git-unstaged-face))))
            modified-unstaged-files)

           ;; Untracked files
           (list
            (cons nil "")
            (cons nil "Untracked:"))

           (mapcar 
            (lambda (file)
              (cons (list (cons 'type 'file) (cons 'state (car file))
                          (cons 'filepath (cdr file)))
                    (format "  %s" (propertize (cdr file) 'face 'circ-git-untracked-face))))
            untracked-files)
           ))))


        

;; ---

(circ-git-get-file-change-states (projectile-ensure-project (projectile-project-root)))
(circ-git-get-current-branch (projectile-ensure-project (projectile-project-root)))
(circ-git-build-status-info (projectile-ensure-project (projectile-project-root)))

(dolist (line
         (circ-git-build-status-info (projectile-ensure-project (projectile-project-root))))
  (message "DEBUG: %s" (cdr line)))

  
(defun circ-git/deduce-files ()
  (message "DEBUG: fileset: %s" (vc-deduce-fileset nil t t))
  (nth 1 (vc-deduce-fileset nil t)))

(defun circ-git/status ()
  (interactive)
  (vc-dir (projectile-ensure-project (projectile-project-root))))

(defun circ-git/add ()
  (interactive)
  (dolist (file (circ-git/deduce-files))
    (circ-git/call-git nil "add" file))
  (revert-buffer))

(defun circ-git/unstage ()
  (interactive)
  (dolist (file (circ-git/deduce-files))
    (circ-git/call-git nil "restore" "--staged" file))
  (revert-buffer))

(defun circ-git/test()
  (interactive)
  (circ-git/deduce-files))
