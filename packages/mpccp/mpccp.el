;;; mpccp.el --- interface to mpc

;; Copyright (C) 2020 Leslie Hor <Leslie.Hor@Gmail.com>

;; Author: Leslie Hor <Leslie.Hor@Gmail.com>
;; Maintainer Leslie Hor <Leslie.Hor@Gmail.com>
;; URL:
;; Keywords: multimedia, mpd, mpc
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
;; An Emacs major mode that provides an interface to mpc.

;;; Code:

(defcustom mpccp-mode-hook nil
  "mpccp hook"
  :group 'mpccp
  :type 'hook)

(defvar mpccp-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `mpccp-mode'.")

(define-derived-mode mpccp-mode special-mode "mpccp"
  "Major mode for mpccp."
  (set (make-local-variable 'revert-buffer-function) #'mpccp))

(define-minor-mode mpccp-database-search-mode
  "Minor mode for mpccp database search"
  :keymap (let ((map (make-sparse-keymap)))
            map))

(define-minor-mode mpccp-queue-mode
  "Minor mode for mpccp database search"
  :keymap (let ((map (make-sparse-keymap)))
            map))

(define-minor-mode mpccp-queue-edit-mode
  "Minor mode for mpccp database search"
  :keymap (let ((map (make-sparse-keymap)))
            map))

(defun evil-mpccp-set-keys ()
  (evil-set-initial-state 'mpccp-mode 'motion)
  (evil-define-key 'motion mpccp-mode-map
    "r" 'mpccp
    "/" 'mpccp-database-search
    "u" 'mpccp-queue
    "U" 'mpccp-update-library
    "q" 'circ/kill-this-buffer
    "l" 'mpccp-load-playlist
    "s" 'mpccp-save-playlist
    "e" 'mpccp-edit-playlist
    "t" 'mpccp-toggle-repeat
    "h" 'mpccp-toggle-shuffle
    "i" 'mpccp-toggle-single
    "c" 'mpccp-toggle-consume
    "," 'mpccp-lower-volume
    "." 'mpccp-raise-volume
    "<" 'mpccp-seek-backward
    ">" 'mpccp-seek-forward
    (kbd "SPC") 'mpccp-play-pause
    "n" 'mpccp-next
    "p" 'mpccp-prev)

  (evil-define-minor-mode-key 'motion 'mpccp-database-search-mode
    "r" 'mpccp-database-search-reload
    "q" 'circ/kill-this-buffer
    "a" 'mpccp-database-search-set-artist-and-search
    "A" 'mpccp-database-search-clear-artist-and-refresh
    "l" 'mpccp-database-search-set-album-and-search
    "L" 'mpccp-database-search-clear-album-and-refresh
    "t" 'mpccp-database-search-set-track-and-search
    "T" 'mpccp-database-search-clear-track-and-refresh
    "p" 'mpccp-database-search-append
    "P" 'mpccp-database-search-append-all
    "i" 'mpccp-database-search-insert
    "I" 'mpccp-database-search-insert-all
    "e" 'mpccp-database-search-replace
    "E" 'mpccp-database-search-replace-all
    "c" 'mpccp-clear-queue
    "Ra" 'mpccp-database-search-random-artist
    "Rl" 'mpccp-database-search-random-album)

  (evil-define-minor-mode-key 'motion 'mpccp-queue-mode
    "r" 'mpccp-queue-reload
    "q" 'circ/kill-this-buffer
    "e" 'mpccp-queue-edit
    "c" 'mpccp-clear-queue
    (kbd "RET") 'mpccp-queue-play-line)

  (evil-define-minor-mode-key 'normal 'mpccp-queue-edit-mode
    "q" 'mpccp-queue-edit-save))
(evil-mpccp-set-keys)

(defgroup mpccp nil
  "mpccp - MPC Control Panel"
  :group 'multimedia)

(defvar mpccp-main-buffer-name "*mpccp-main*"
  "Name for the mpccp buffer")

(defvar mpccp-database-search-buffer-name "*mpccp-database-search*"
  "Name for the mpccp database search buffer")
(defvar mpccp-queue-buffer-name "*mpccp-queue*"
  "Name for the mpccp queue buffer")

(defvar mpccp-results-list-offset 22) ;TODO Find this programmatically
(defvar mpccp-database-search-albumartist-length 0.2)
(defvar mpccp-database-search-artist-length      0.2)
(defvar mpccp-database-search-album-length       0.2)
(defvar mpccp-database-search-title-length       0.6)

(defvar-local mpccp-mpc-status nil
  "Stores various mpc toggle states")
(defvar-local mpccp-song-alist nil
  "Stores the song results from searches or playlists")
(defvar-local mpccp-database-search-artist ""
  "Stores the last search string for artist")
(defvar-local mpccp-database-search-album ""
  "Stores the last search string for album")
(defvar-local mpccp-database-search-track ""
  "Stores the last search string for track")

(defface mpccp-header-1
  '((t :inherit font-lock-keyword-face :bold t :underline t))
  "Main header style"
  :group 'mpccp)

(defface mpccp-header-2
  '((t :inherit font-lock-keyword-face :bold t))
  "Sub header style"
  :group 'mpccp)

(defvar mpccp-format-all-data
  (string-join '("%artist%" "%album% ""%albumartist%" "%date%" "%disc%"
                 "%genre%" "%title%" "%track%" "%time%" "%file%" "%position%")
               "\t")
  "Format string to gather all useful metadata. Changing this will break things.")

(defun mpccp-call-mpc (destination args)
  "Call mpc with args"
  (if (not (listp args))
      (setq args (list args)))
  (apply 'call-process "mpc" nil destination nil args))

(defun mpccp-parse-metadata-format (metadata)
  "Convert mpc string into list format"
  (cl-destructuring-bind (artist album albumartist date disc genre title track
                                 time file position id) metadata
    '(artist album albumartist date (string-to-number disc) genre title
             (string-to-number track) time file (string-to-number position)
             (string-to-number id))))

(defun mpccp-update-library ()
  (interactive)
  "Update library"
  (mpccp-call-mpc nil '("update" "--wait"))
  (message "Library updated"))

(defun mpccp-get-current-song ()
  "Get current song."
  (with-temp-buffer
    (mpccp-call-mpc t '("current" "--format" "%artist% - %album% - %title%"))
    (string-trim (buffer-string))))

(defun mpccp-get-current-song-progress ()
  "Get current song progress."
  (interactive)
  (with-temp-buffer
    (let* ((mpc-status (with-temp-buffer
                         (mpccp-call-mpc t '("status"))
                         (buffer-string)))
           (mpc-split-status (split-string mpc-status "\n"))
           (mpc-status-line (replace-regexp-in-string "  *" "\t"
                                                      (nth 1 mpc-split-status)))
           (mpc-status-list (split-string mpc-status-line "\t"))
           (mpc-song-progress (nth 2 mpc-status-list)))
      (cond (mpc-song-progress mpc-song-progress)
            (t "0")))))

(defun mpccp-get-current-song-queue-position ()
  "Get current song queue position"
  (interactive)
  (with-temp-buffer
    (mpccp-call-mpc t '("current" "--format" "%position%"))
    (string-to-number (string-trim (buffer-string)))))

(defun mpccp-get-queue-length ()
  (interactive)
  (with-temp-buffer
    (mpccp-call-mpc t '("playlist"))
    (count-lines (point-min) (point-max))))

(defun mpccp-off-on-to-bool (s)
  (cond ((string= s "on") t)
        (t nil)))

(defun mpccp-pretty-bool (b)
  (cond (b "Yes")
        (t "No")))

(defun mpccp-get-status-info ()
  (setq mpccp-mpc-status
        (let* ((mpc-status (with-temp-buffer
                             (mpccp-call-mpc t '("status"))
                             (buffer-string)))
               (mpc-split-status (split-string mpc-status "\n"))
               (mpc-status-line (replace-regexp-in-string "[: ] *" "\t"
                                                          (nth
                                                           (- (length mpc-split-status) 2)
                                                           mpc-split-status)))
               (mpc-status-list (split-string mpc-status-line "\t"))
               (mpc-volume (replace-regexp-in-string "%" "" (nth 1 mpc-status-list)))
               (mpc-repeat-state (nth 3 mpc-status-list))
               (mpc-shuffle-state (nth 5 mpc-status-list))
               (mpc-single-state (nth 7 mpc-status-list))
               (mpc-consume-state (nth 9 mpc-status-list)))
          (list
           (cons 'volume (string-to-number mpc-volume))
           (cons 'repeat (mpccp-off-on-to-bool mpc-repeat-state))
           (cons 'shuffle (mpccp-off-on-to-bool mpc-shuffle-state))
           (cons 'single (mpccp-off-on-to-bool mpc-single-state))
           (cons 'consume (mpccp-off-on-to-bool mpc-consume-state))))))

(defun mpccp-status-print-volume () (number-to-string (alist-get 'volume mpccp-mpc-status)))
(defun mpccp-status-print-repeat () (mpccp-pretty-bool (alist-get 'repeat mpccp-mpc-status)))
(defun mpccp-status-print-shuffle () (mpccp-pretty-bool (alist-get 'shuffle mpccp-mpc-status)))
(defun mpccp-status-print-single () (mpccp-pretty-bool (alist-get 'single mpccp-mpc-status)))
(defun mpccp-status-print-consume () (mpccp-pretty-bool (alist-get 'consume mpccp-mpc-status)))

(defun mpccp-toggle-repeat ()
  (interactive)
  (mpccp-call-mpc nil '("repeat"))
  (mpccp))
(defun mpccp-toggle-shuffle ()
  (interactive)
  (mpccp-call-mpc nil '("random"))
  (mpccp))
(defun mpccp-toggle-single ()
  (interactive)
  (mpccp-call-mpc nil '("single"))
  (mpccp))
(defun mpccp-toggle-consume ()
  (interactive)
  (mpccp-call-mpc nil '("consume"))
  (mpccp))

(defun mpccp-raise-volume ()
  (interactive)
  (mpccp-call-mpc nil '("volume" "+5"))
  (mpccp))
(defun mpccp-lower-volume ()
  (interactive)
  (mpccp-call-mpc nil '("volume" "-5"))
  (mpccp))

(defun mpccp-seek-backward ()
  (interactive)
  (mpccp-call-mpc nil '("seek" "-00:00:05")))
(defun mpccp-seek-forward ()
  (interactive)
  (mpccp-call-mpc nil '("seek" "+00:00:05")))

(defun mpccp-play-pause ()
  (interactive)
  (mpccp-call-mpc nil '("toggle"))
  (mpccp))

(defun mpccp-next ()
  (interactive)
  (mpccp-call-mpc nil '("next"))
  (mpccp))
(defun mpccp-prev ()
  (interactive)
  (mpccp-call-mpc nil '("prev"))
  (mpccp))

(defun mpccp-parse-mpc-song-to-alist (song)
  (cl-destructuring-bind (artist album albumartist date disc genre title
                                track time file position) song
    (list
     (cons 'artist artist)
     (cons 'album album)
     (cons 'albumartist albumartist)
     (cons 'date date)
     (cons 'disc disc)
     (cons 'genre genre)
     (cons 'title title)
     (cons 'track track)
     (cons 'time time)
     (cons 'file file)
     (cons 'position position))))

(defun mpccp-parse-mpc-song-list (song-list)
  (let (indexed-list
        (counter 0))
    (dolist (song (mapcar
                   (lambda (item) (split-string item "\t"))
                   (split-string (string-trim song-list) "\n")))
      (setq counter (+ counter 1))
      (setq indexed-list (cons
                          (cons
                           counter
                           (mpccp-parse-mpc-song-to-alist song))
                           indexed-list)))
    (reverse indexed-list)))

(defun mpccp-search (artist album track)
  (cond ((or (not (eq artist ""))
             (not (eq album ""))
             (not (eq track "")))
         (with-temp-buffer
           (mpccp-call-mpc t (list
                              "--format" mpccp-format-all-data
                              "search"
                              "artist" artist
                              "album" album
                              "title" track))
           (mpccp-parse-mpc-song-list (buffer-string))))
        (t "")))

(defun mpccp-list-playlists ()
  (with-temp-buffer
    (mpccp-call-mpc t "lsplaylists")
    (split-string (buffer-string) "\n")))

(defun mpccp-mpc-load-playlist (playlist)
  (mpccp-call-mpc nil (list "load" playlist)))

(defun mpccp-mpc-save-playlist (playlist)
  (mpccp-call-mpc nil (list "rm" playlist))
  (mpccp-call-mpc nil (list "save" playlist)))

(defvar mpccp-playlist-directory nil
  "Directory that contains MPD playlists. Required for editing plyalists")

(defun mpccp-mpc-edit-playlist (playlist)
  (cond (mpccp-playlist-directory
         (find-file (concat (file-name-as-directory mpccp-playlist-directory) playlist)))
        (t
         (user-error "mpccp-playlist-directory not set"))))

(defun mpccp-mpc-queue ()
  (with-temp-buffer
    (mpccp-call-mpc t (list
                       "--format" mpccp-format-all-data
                       "playlist"))
    (mpccp-parse-mpc-song-list (buffer-string))))

(defun mpccp-database-search-perform-search ()
  (setq mpccp-song-alist (mpccp-search mpccp-database-search-artist
                                       mpccp-database-search-album
                                       mpccp-database-search-track)))

(defun mpccp-load-queue ()
  (setq mpccp-song-alist (mpccp-mpc-queue)))

(defun mpccp-trunc-pad-string (length string)
  (truncate-string-to-width string length 0 ?\s))
(defun mpccp-pretty-song-list ()
  (let* ((avail-line-len (- (window-text-width) mpccp-results-list-offset))
         (artist-len (floor (* avail-line-len mpccp-database-search-artist-length)))
         (album-len  (floor (* avail-line-len mpccp-database-search-album-length)))
         (title-len  (floor (* avail-line-len mpccp-database-search-title-length))))
    (string-join
     (mapcar
      (lambda (song-data)
        (let ((index (car song-data))
              (song (cdr song-data)))
          (format "%3s) | %3s | %s | %s | %s |"
                  index
                  (alist-get 'track song)
                  (mpccp-trunc-pad-string artist-len
                                          (alist-get 'artist song))
                  (mpccp-trunc-pad-string album-len
                                          (alist-get 'album song))
                  (mpccp-trunc-pad-string title-len
                                          (alist-get 'title song)))))
      mpccp-song-alist)
     "\n")))

(defun mpccp-queue-raw-file ()
  (string-join
   (mapcar
    (lambda (song-data)
      (let ((song (cdr song-data)))
        (alist-get 'file song)))
    mpccp-song-alist)
   "\n"))

(defun mpccp-database-search-query-defined-p ()
  (or (not (eq mpccp-database-search-artist ""))
      (not (eq mpccp-database-search-album ""))
      (not (eq mpccp-database-search-track ""))))

(defun mpccp-get-artists ()
  (with-temp-buffer
    (mpccp-call-mpc t '("listall" "--format" "%artist%"))
    (delete-dups (split-string (buffer-string) "\n"))))
(defun mpccp-get-album-artists ()
  (with-temp-buffer
    (mpccp-call-mpc t '("listall" "--format" "%albumartist%"))
    (delete-dups (split-string (buffer-string) "\n"))))
(defun mpccp-database-search-clear-artist ()
  (interactive)
  (setq mpccp-database-search-artist ""))
(defun mpccp-database-search-set-artist (&optional artist)
  (interactive)
  (let ((ar (cond (artist artist)
                  (t (completing-read "Artist: " (mpccp-get-album-artists))))))
    (setq mpccp-database-search-artist ar)))
(defun mpccp-database-search-set-artist-and-search ()
  (interactive)
  (mpccp-database-search-set-artist)
  (mpccp-database-search-reload))
(defun mpccp-database-search-clear-artist-and-refresh()
  (interactive)
  (mpccp-database-search-clear-artist)
  (mpccp-database-search-reload))

(defun mpccp-get-albums ()
  (let ((artist mpccp-database-search-artist))
    (with-temp-buffer
      (mpccp-call-mpc t (list
                         "--format" "%album%"
                         "search" "artist" artist))
      (delete-dups (split-string (buffer-string) "\n")))))
(defun mpccp-database-search-clear-album ()
  (interactive)
  (setq mpccp-database-search-album ""))
(defun mpccp-database-search-set-album (&optional album)
  (interactive)
  (let ((al (cond (album album)
                  (t (completing-read "Album: " (mpccp-get-albums))))))
    (setq mpccp-database-search-album al)))
(defun mpccp-database-search-set-album-and-search ()
  (interactive)
  (mpccp-database-search-set-album)
  (mpccp-database-search-reload))
(defun mpccp-database-search-clear-album-and-refresh()
  (interactive)
  (mpccp-database-search-clear-album)
  (mpccp-database-search-reload))

(defun mpccp-database-search-random-artist ()
  (interactive)
  (mpccp-database-search-clear-artist)
  (mpccp-database-search-clear-album)
  (mpccp-database-search-clear-track)
  (mpccp-database-search-set-artist (seq-random-elt (mpccp-get-album-artists)))
  (mpccp-database-search-reload))

(defun mpccp-database-search-random-album ()
  (interactive)
  (mpccp-database-search-clear-artist)
  (mpccp-database-search-clear-album)
  (mpccp-database-search-clear-track)
  (mpccp-database-search-set-album (seq-random-elt (mpccp-get-albums)))
  (mpccp-database-search-reload))

(defun mpccp-get-tracks ()
  (let ((artist mpccp-database-search-artist)
        (album mpccp-database-search-album))
    (with-temp-buffer
      (mpccp-call-mpc t (list
                         "--format" "%title%"
                         "search" "artist" artist
                         "album" album))
      (delete-dups (split-string (buffer-string) "\n")))))
(defun mpccp-database-search-clear-track ()
  (interactive)
  (setq mpccp-database-search-track ""))
(defun mpccp-database-search-set-track ()
  (interactive)
  (setq mpccp-database-search-track
        (completing-read "Track: " (mpccp-get-tracks))))
(defun mpccp-database-search-set-track-and-search ()
  (interactive)
  (mpccp-database-search-set-track)
  (mpccp-database-search-reload))
(defun mpccp-database-search-clear-track-and-refresh ()
  (interactive)
  (mpccp-database-search-clear-track)
  (mpccp-database-search-reload))

(defun mpccp-add-paths (paths)
  (dolist (path paths)
    (mpccp-call-mpc nil (list "add" path))))
(defun mpccp-add-path (path)
  (mpccp-call-mpc nil (list "add" path)))
(defun mpccp-insert-paths (paths)
  (dolist (path (reverse paths))
    (mpccp-call-mpc nil (list "insert" path))))
(defun mpccp-insert-path (path)
  (mpccp-call-mpc nil (list "insert" path)))
(defun mpccp-clear-queue ()
  (interactive)
  (mpccp-call-mpc nil "clear")
  (message "Queue cleared"))
(defun mpccp-play ()
  (mpccp-call-mpc nil "play"))
(defun mpccp-pause ()
  (mpccp-call-mpc nil "pause"))

(defun mpccp-database-search-get-song-alist-filepaths ()
  (mapcar (lambda (song) (alist-get 'file song)) mpccp-song-alist))
(defun mpccp-database-search-get-current-line-filepath ()
  (let ((index (string-to-number
                (car
                 (split-string
                  (string-trim (thing-at-point 'line t))
                  ")")))))
    (alist-get 'file
               (alist-get index mpccp-song-alist))))

(defun mpccp-database-search-append ()
  (interactive)
  (mpccp-add-path (mpccp-database-search-get-current-line-filepath))
  (message "Song appended"))
(defun mpccp-database-search-append-all ()
  (interactive)
  (mpccp-add-paths (mpccp-database-search-get-song-alist-filepaths))
  (message "Songs appended"))

(defun mpccp-database-search-insert ()
  (interactive)
  (mpccp-insert-path (mpccp-database-search-get-current-line-filepath))
  (message "Song inserted"))
(defun mpccp-database-search-insert-all ()
  (interactive)
  (mpccp-insert-paths (mpccp-database-search-get-song-alist-filepaths))
  (message "Songs inserted"))

(defun mpccp-database-search-replace ()
  (interactive)
  (mpccp-clear-queue)
  (mpccp-add-path (mpccp-database-search-get-current-line-filepath))
  (message "Song replaced")
  (mpccp-play))
(defun mpccp-database-search-replace-all ()
  (interactive)
  (mpccp-clear-queue)
  (mpccp-add-paths (mpccp-database-search-get-song-alist-filepaths))
  (message "Songs replaced")
  (mpccp-play))

(defun mpccp-load-playlist ()
  (interactive)
  (let ((playlist (completing-read "Load Playlist: " (mpccp-list-playlists))))
    (mpccp-clear-queue)
    (mpccp-mpc-load-playlist playlist)
    (mpccp-play)))

(defun mpccp-save-playlist ()
  (interactive)
  (let ((playlist (completing-read "Save Playlist: " (mpccp-list-playlists))))
    (mpccp-mpc-save-playlist playlist)))

(defun mpccp-edit-playlist ()
  (interactive)
  (cond (mpccp-playlist-directory
         (let ((playlist (completing-read "Edit Playlist: " (directory-files mpccp-playlist-directory nil "m3u"))))
           (mpccp-mpc-edit-playlist playlist)))
        (t
         (user-error "mpccp-playlist-directory not set"))))

(defun mpccp-update-current-song ()
  (interactive)
  (save-excursion
    (let ((buf (get-buffer mpccp-main-buffer-name)))
      (cond (buf
             (with-current-buffer buf
               (read-only-mode -1)
               (goto-char (point-min))
               (search-forward "    Current: ")
               (delete-region (line-beginning-position) (+ 1 (line-end-position)))
               (insert "    Current: " (mpccp-get-current-song) "\n")
               (search-forward "    Progress: ")
               (delete-region (line-beginning-position) (+ 1 (line-end-position)))
               (insert "    Progress: " (number-to-string (mpccp-get-current-song-queue-position))
                       "/" (number-to-string (mpccp-get-queue-length)) " - "
                       (mpccp-get-current-song-progress) "\n")))
            (t
             (mpccp-cancel-update-timer))))))

(defun mpccp-cancel-update-timer ()
  (message "Cancelling mpccp update loop")
  (cancel-function-timers 'mpccp-update-current-song))

(defun mpccp-run-update-timer ()
  "Starts the mpccp timer. If the timer is already running, it
does not try to run it"
  (unless (member 'mpccp-update-current-song
                  (mapcar (lambda (timer)
                            (timer--function timer))
                          timer-list))
    (message "Starting mpccp update loop")
    (run-with-timer 1 1 'mpccp-update-current-song)))

(defun mpccp (&optional ignore-auto noconfirm)
  (interactive)
  (when (= (call-process "mpc") 1)
    (when (y-or-n-p "MPC unable to connect. Start mpd?")
      (call-process "mpd")))
  (let ((buf (get-buffer-create mpccp-main-buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (mpccp-get-status-info)
      (insert (propertize "MPC Control Panel - Home\n" 'face 'mpccp-header-1)
              "[r] reload" " " "[q] quit" "\n"
              "\n"
              "* indicates non implemented things\n"
              "\n"
              (propertize "Status\n" 'face 'mpccp-header-2)
              "    Current: " (mpccp-get-current-song) "\n"
              "    Progress: " (number-to-string (mpccp-get-current-song-queue-position))
              "/" (number-to-string (mpccp-get-queue-length)) " - "
              (mpccp-get-current-song-progress) "\n"
              "    Volume: " (mpccp-status-print-volume) "%\n"
              "\n"
              (propertize "Player Commands\n" 'face 'mpccp-header-2)
              "    [SPACE] Play / Pause\n"
              "    [p] Prev\n"
              "    [n] Next\n"
              "    [,] Volume -5%\n"
              "    [.] Volume +5%\n"
              "    [<] Seek -5 sec\n"
              "    [>] Seek +5 sec\n"
              "    [t] Repeat  - " (mpccp-status-print-repeat) "\n"
              "    [h] Shuffle - " (mpccp-status-print-shuffle) "\n"
              "    [i] Single  - " (mpccp-status-print-single) " (plays one song then pauses) \n"
              "    [c] Consume - " (mpccp-status-print-consume) "\n"
              "\n"
              (propertize "Database\n" 'face 'mpccp-header-2)
              "    [/] Search\n"
              "\n"
              (propertize "Queue\n" 'face 'mpccp-header-2)
              "    [u] Queue\n"
              "    [l] Load Playlist to Queue\n"
              "    [s] Save Queue as Playlist\n"
              "    [e] Edit Playlist\n"
              "\n"
              (propertize "Other\n" 'face 'mpccp-header-2)
              "    [U] Update library\n"
              "    [r] reload\n"
              "    [q] quit"
              )
      (mpccp-mode)
      (mpccp-run-update-timer)
      (switch-to-buffer buf)
      (run-hooks 'mpccp-mode-hook))))

(defun mpccp-format-results-header ()
  (let* ((avail-line-len (- (window-text-width) mpccp-results-list-offset))
         (artist-len (floor (* avail-line-len mpccp-database-search-artist-length)))
         (album-len  (floor (* avail-line-len mpccp-database-search-album-length)))
         (title-len  (floor (* avail-line-len mpccp-database-search-title-length))))
    (format " ID  | No  | %s | %s | %s |\n"
            (mpccp-trunc-pad-string artist-len "Artist")
            (mpccp-trunc-pad-string album-len "Album")
            (mpccp-trunc-pad-string title-len "Title"))))

(defun mpccp-database-search-buffer-name ()
  mpccp-database-search-buffer-name)

(defun mpccp-database-search-reload ()
  (interactive)
  (let ((buf (get-buffer-create mpccp-database-search-buffer-name)))
    (with-current-buffer buf
      (mpccp-database-search-perform-search)
      (erase-buffer)
      (insert (propertize "MPC Control Panel - Database Search\n" 'face 'mpccp-header-1)
              "[r] reload" " " "[q] quit" "\n"
              "\n"
              "[A] clear [a] Artist: " mpccp-database-search-artist "\n"
              "[L] clear [l] Album:  " mpccp-database-search-album "\n"
              "[T] clear [t] Track:  " mpccp-database-search-track "\n"
              "[Ra] Random artist\n"
              "[Rl] Random album\n"
              "\n"
              "[c] Clear queue\n"
              "[p] Append" "     " "[i] Insert" "     " "[e] Replace" "\n"
              "[P] Append all" " " "[I] Insert all" " " "[E] Replace all" "\n"
              "\n"
              (propertize "Results\n" 'face 'mpccp-header-2)
              (propertize (mpccp-format-results-header)
                          'face 'mpccp-header-2)
              (mpccp-pretty-song-list))
      (goto-char (point-min))
      (search-forward "Results")
      (move-beginning-of-line 3))))

(defun mpccp-database-search ()
  (interactive)
  (let ((buf (get-buffer-create mpccp-database-search-buffer-name)))
    (with-current-buffer buf
      (mpccp-mode)
      (mpccp-database-search-mode)
      (read-only-mode -1)
      (mpccp-database-search-reload)
      (switch-to-buffer buf)
      (hl-line-mode 1))))

(defun mpccp-queue-play-line ()
  (interactive)
  (let ((index (string-to-number
                (car
                 (split-string
                  (string-trim (thing-at-point 'line t))
                  ")")))))
    (mpccp-call-mpc nil (list "play" (number-to-string index)))))

(defun mpccp-queue-edit ()
  (interactive)
  (let ((buf (get-buffer-create "*queue-test*")))
    (with-current-buffer buf
      (text-mode) ; So we can edit the queue
      (mpccp-queue-edit-mode)
      (read-only-mode -1)
      (erase-buffer)
      (mpccp-load-queue)
      (insert "[q] Save and close" " " "[c] Cancel\n"
              "-\n"
              (mpccp-queue-raw-file))
      (switch-to-buffer buf)
      (goto-char (point-min))
      (move-beginning-of-line 3))))

(defun mpccp-queue-edit-save ()
  (interactive)
  (goto-line 3)
  (let ((file-list (buffer-substring (point) (point-max))))
    (mpccp-clear-queue)
    (mpccp-add-paths (split-string file-list "\n")))
  (kill-this-buffer)
  (mpccp-queue-reload))

(defun mpccp-queue-edit-no-save ()
  (interactive)
  (message "Cancelling")
  (kill-this-buffer)
  (mpccp-queue-reload))

(defun mpccp-queue-reload ()
  (interactive)
  (let ((buf (get-buffer-create mpccp-queue-buffer-name)))
    (with-current-buffer buf
      ;; Get queue data
      (mpccp-load-queue)
      (erase-buffer)
      (insert (propertize "MPC Control Panel - Queue\n" 'face 'mpccp-header-1)
              "[r] reload" " " "[q] quit" "\n"
              "\n"
              "[c] Clear queue" " " "[e] Edit" " " "[RET] Play song" "\n"
              "\n"
              "Feel free to edit this buffer and commit it\n"
              (propertize "Queue\n" 'face 'mpccp-header-2)
              (propertize (mpccp-format-results-header)
                          'face 'mpccp-header-2)
              (mpccp-pretty-song-list)
              ; get queue listing
              )
      (goto-char (point-min))
      (search-forward "\nQueue")
      (move-beginning-of-line 3))))

(defun mpccp-queue ()
  (interactive)
  (let ((buf (get-buffer-create mpccp-queue-buffer-name)))
    (with-current-buffer buf
      (mpccp-mode)
      (mpccp-queue-mode)
      (read-only-mode -1)
      (mpccp-queue-reload)
      (switch-to-buffer buf)
      (hl-line-mode 1))))

(provide 'mpccp)
