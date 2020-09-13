;;; dashboard.el --- A minimal dashboard

;; Copyright (C) 2020 Leslie Hor <Leslie.Hor@Gmail.com>

;; Author: Leslie Hor <Leslie.Hor@Gmail.com>
;; Maintainer Leslie Hor <Leslie.Hor@Gmail.com>
;; URL:
;; Keywords: dashboard, initial-buffer
;; Version: 1.0
;; Package-Requires: ()

;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; A simple dashboard for emacs

;;; Code:

(require 'recentf)

(defvar dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `dashboard-mode'.")

(define-derived-mode dashboard-mode special-mode "dashboard"
  "Major mode for dashboard."
  (set (make-local-variable 'revert-buffer-function) #'dashboard))

(defface dashboard-header-1
  '((t :inherit font-lock-keyword-face :height 2.0 :bold t :underline t))
  "Main header style"
  :group 'dashboard)

(defface dashboard-header-2
  '((t :inherit font-lock-keyword-face :height 1.5 :bold t))
  "Sub header style"
  :group 'dashboard)

(defface dashboard-bold
  '((t :inherit font-lock-keyword-face :bold t))
  "Bold style"
  :group 'dashboard)

(defface dashboard-abbrev
  '((t :inherit nil :foreground "#555555"))
  "Abbrev style"
  :group 'dashboard)

(defface dashboard-location
  '((t :inherit nil))
  "Location style"
  :group 'dashboard)

(defun evil-dashboard-set-keys ()
  (evil-set-initial-state 'dashboard-mode 'motion)
  (evil-define-key 'motion dashboard-mode-map
    (kbd "r") 'dashboard
    (kbd "o") 'dashboard-open-link
    (kbd "RET") 'dashboard-open-link

    (kbd "a") 'circ/alias-org-agenda-agenda
    (kbd "t") 'circ/alias-org-agenda-tasks
    (kbd "i") 'circ/open-inbox
    (kbd "l") 'circ/open-checklists
    (kbd "e") 'elfeed
    (kbd "m") 'mpccp
    )
  )
(evil-dashboard-set-keys)

(defvar dashboard-link-abbrev-alist
  '(("file" . find-file)
    ("url" . browse-url)
    ("buf" . switch-to-buffer)
    ("dir" . find-file))
  "How to parse links.
Each item is a cons in the form (ABBREV, FUN).

FUN is called with the string as the arg.

Example:
file:~/.emacs.d/init.el
will call `find-file' with \"~/.emacs.d/init.el\" as the arg")

(defvar dashboard-persistent-links '()
  "Links to always show in the dashboard.")

(defun dashboard-get-abbrev (string)
  (car (split-string string  ":")))
(defun dashboard-get-location (string)
  (string-join (cdr (split-string string ":")) ":"))

(defun dashboard-get-recentf-list ()
  (cond ((boundp 'recentf-list)
         (mapcar (lambda (file)
                   (concat "file:"
                           (replace-regexp-in-string
                            abbreviated-home-dir
                            "~/" file)))
                 recentf-list))
        (t '())))

(defun dashboard-get-buffer-list ()
  (mapcar (lambda (buffer)
            (concat "buf" ":" (buffer-name buffer)))
          ;; Remove buffers whose name are prefixed with a space.
          (remove-if (lambda (buffer)
                       (string= (substring (buffer-name buffer)
                                           0 1)
                                " "))
                     (buffer-list))))

(defun dashboard-print-links-list (list)
  (concat (string-join
           (mapcar (lambda (item)
                     (let ((abbrev (dashboard-get-abbrev item))
                           (location (dashboard-get-location item)))
                       (concat
                        (propertize abbrev 'face 'dashboard-abbrev)
                        (propertize ":" 'face 'dashboard-abbrev)
                        (propertize location 'face 'dashboard-location))))
                   list)
           "\n") "\n"))

(defun dashboard-open-link ()
  (interactive)
  (let* ((current-line (string-trim (thing-at-point 'line t)))
         (abbrev (dashboard-get-abbrev current-line))
         (location (dashboard-get-location current-line))
         (fun  (cdr (assoc abbrev dashboard-link-abbrev-alist))))
    (funcall fun location)))

(defun dashboard (&optional ignore-auto noconfirm)
  (interactive)
  (let ((buf (get-buffer-create "*dashboard*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (recentf-load-list)
      (insert (propertize "Dashboard\n" 'face 'dashboard-header-1)
              "\n"
              " [o] Open link       [r] Reload          [q] Quit\n"
              "\n"
              (propertize "Shortcuts\n" 'face 'dashboard-header-2)
              " [a] Agenda          [t] Tasks           [i] Inbox           [l] Checklists\n"
              " [e] Elfeed          [m] Mpccp\n"
              "\n"
              (propertize "Persistent Links\n" 'face 'dashboard-header-2)
              (dashboard-print-links-list dashboard-persistent-links)
              "\n"
              (propertize "Recent Files\n" 'face 'dashboard-header-2)
              (dashboard-print-links-list (dashboard-get-recentf-list))
              "\n"
              (propertize "Buffer List\n" 'face 'dashboard-header-2)
              (dashboard-print-links-list (dashboard-get-buffer-list))
              )
      (dashboard-mode)
      (switch-to-buffer buf)
      (goto-char (point-min))
      (search-forward "Persistent Links")
      (move-beginning-of-line 2)
      (hl-line-mode))))

(provide 'dashboard)
