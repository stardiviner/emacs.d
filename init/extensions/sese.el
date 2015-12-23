;; Copyright (c) 2008 Free Software Foundation, Inc.

;; Author: Mihai Bazon  / http://mihai.bazon.net/blog
;; URL: http://mihai.bazon.net/projects/sese

;; Based on code and ideas from GNEVE / http://www.1010.co.uk/gneve.html

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; This is a major mode for writing subtitles
;;
;; It uses MPlayer's "slave" feature in order to playback the movie
;; and automatically fetch timing information.
;;
;; * Installation:
;;
;; Put sese.el into some directory in your Emacs library path, and put
;; the following into your .emacs
;;
;;     (autoload 'sese-mode "sese" "Subtitle Editor major mode" t)
;;     (setq auto-mode-alist
;;           (cons '("\\.sese\\'" . sese-mode) auto-mode-alist))
;;
;; Then restart Emacs.
;;
;; * Creating subtitles
;;
;; Create a new file named MovieName.sese (for convenience, put it
;; into the folder where you have the movie).  You can simply use C-x
;; C-f to open this file.
;;
;; Press C-? (control shift /).  You will be asked for the video file
;; name.  After you enter it, mplayer starts up for that video.  Make
;; sure you focus the Emacs window now (mplayer should stay on top).
;;
;; All you have to do now is to watch the movie.  Press C-ENTER to
;; mark the start of a subtitle.  Without moving the caret, press
;; C-ENTER again to mark the end of a subtitle.  SESE will
;; automatically insert timing information and a pair of brackets,
;; with the caret between.  The second time you pressed C-ENTER, the
;; movie is paused so you can take your time to write the subtitles.
;;
;; When all is ready, generate SRT subtitles with:
;;
;;      M-x sese-render-subtitles
;;
;; That's it, enjoy!  There are more interesting keyboard shortcuts,
;; visit http://mihai.bazon.net/projects/sese for details and/or take
;; a look at the source code in sese.el.

(require 'font-lock)
(require 'filladapt)

(defgroup sese nil
  "Major mode for creating subtitles"
  :group 'languages
  :group 'hypermedia
  :prefix "sesem-")

(defcustom sesem-mplayer-vo "gl"
  "*MPlayer video output driver (argument to -vo)"
  :type 'string
  :group 'sese)

(defcustom sesem-mplayer-xy 400
  "*MPlayer window width (argument to -xy)"
  :type 'integer
  :group 'sese)

(defvar sese-mode-map nil
  "SESE local keymap")

(defvar sese-process-buffer "*sese-process*")
(defvar sese-process "sese-process")

(defvar sese-mode-hook nil)

(defconst sese-number-regexp
  "\\(-?\\([0-9]+\\.?\\|\\.\\)[0-9]*\\(e[0-9]+\\)?\\)")

(defconst sese-start-regexp
  (concat "^" sese-number-regexp "\\ *-\\ *"))

(defconst sese-fragment-regexp
  (concat sese-start-regexp sese-number-regexp))

(defconst sese-movie-line-regexp "^#\\ *MOVIE:\\ *\\(.*?\\)\\ *$")

(defvar sese-font-lock-keywords
  (list
   `(,sese-fragment-regexp
     (1 font-lock-keyword-face t)
     (4 font-lock-reference-face t))
   (cons "^#.*$" font-lock-comment-face)
   (cons "^\t.*$" font-lock-string-face))
  "SESE font lock rules")

(setq sese-mode-map nil)

(unless sese-mode-map
  (setq sese-mode-map (make-sparse-keymap))

  (define-key sese-mode-map [(control return)]  'sese-mark-subtitle)
  (define-key sese-mode-map [(control \,)]      'sese-video-rew-1-sec)
  (define-key sese-mode-map [(control \.)]      'sese-video-fwd-1-sec)
  (define-key sese-mode-map [(meta \,)]         'sese-video-rew-10-sec)
  (define-key sese-mode-map [(meta \.)]         'sese-video-fwd-10-sec)
  (define-key sese-mode-map [(control p)]       'sese-video-pause)
  (define-key sese-mode-map [(control \])]      'sese-video-replay)
  (define-key sese-mode-map [(return)]          'sese-newline)
  (define-key sese-mode-map [(control \?)]      'sese-open-this-video)
  (define-key sese-mode-map [(meta \[)]         '(lambda() (interactive) (sese-set-marker-pos 1))) ; set start position
  (define-key sese-mode-map [(meta \])]         '(lambda() (interactive) (sese-set-marker-pos 4))) ; set end position
  )

(defun sese-open-video (filename)
  (interactive "fFilename: ")
  (save-excursion
    (set-buffer (get-buffer-create sese-process-buffer)) ; starts with a space to disable undo!
    (buffer-disable-undo)
    (erase-buffer)
    (start-process sese-process sese-process-buffer
                   "mplayer"
                   "-slave"
                   "-vo" sesem-mplayer-vo
                   "-xy" (number-to-string sesem-mplayer-xy)
                   "-ontop"
                   "-osdlevel" "1"
                   "-quiet"
                   (expand-file-name filename)))
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward sese-movie-line-regexp nil t)
        (unless (string= (match-string 1) filename)
          (goto-char (match-beginning 1))
          (delete-char (length (match-string 1)))
          (insert filename))
      (insert "# MOVIE: " filename "\n\n"))))

(defun sese-open-this-video ()
  (interactive)
  (let ((filename (sese-get-video-file-name)))
    (if filename
        (sese-open-video filename)
      (call-interactively 'sese-open-video))))

(defun sese-get-video-file-name ()
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward sese-movie-line-regexp nil t)
        (match-string 1)
      nil)))

(defun sese-mark-subtitle ()
  (interactive)
  (if (looking-back sese-start-regexp)
      (progn
        (insert (sese-get-position) " {\n\t\n}")
        (sese-video-pause)
        (backward-char 2))
    (progn
      (unless (looking-back "^")
        (re-search-forward "^}")
        (insert "\n\n"))
      (insert (sese-get-position) " - "))))

(defun sese-set-marker-pos (which)
  (save-excursion
    (let ((pos (sese-get-position))
          (p (parse-partial-sexp (point-min) (point)))
          start end)
      (if (= 0 (nth 0 p))
          (message "Not in subtitle text")
        (goto-char (nth 1 p))
        (skip-chars-backward " \t")
        (if (not (looking-back sese-fragment-regexp))
            (message "Couldn't find START-END markers")
          (setq start (match-beginning which)
                end (match-end which))
          (goto-char start)
          (delete-char (- end start))
          (insert pos))))))

(defun sese-video-rew-1-sec ()
  (interactive)
  (process-send-string sese-process "seek -2.0\n"))

(defun sese-video-fwd-1-sec ()
  (interactive)
  (process-send-string sese-process "seek 1.0\n"))

(defun sese-video-rew-5-sec ()
  (interactive)
  (process-send-string sese-process "seek -6.0\n"))

(defun sese-video-fwd-5-sec ()
  (interactive)
  (process-send-string sese-process "seek 5.0\n"))

(defun sese-video-rew-10-sec ()
  (interactive)
  (process-send-string sese-process "seek -11.0\n"))

(defun sese-video-fwd-10-sec ()
  (interactive)
  (process-send-string sese-process "seek 10.0\n"))

(defun sese-video-pause ()
  (interactive)
  (process-send-string sese-process "pause\n"))

(defun sese-video-absolute-seek (pos)
  (if (numberp pos)
      (setq pos (number-to-string pos)))
  (process-send-string sese-process (format "seek %s 2\n" pos)))

(defun sese-video-replay ()
  (interactive)
  (save-excursion
    (re-search-backward sese-start-regexp)
    (sese-video-absolute-seek (match-string 1))))

;; this is kind of dumb, I should define a proper indent function
(defun sese-newline ()
  (interactive)
  (if (looking-back "^\t.*")
      (progn
        (newline)
        (insert "\t"))
    (newline)))

(defun sese-seconds-to-time (s)
  (let (hours minutes seconds millisecons)
    (setq hours (floor (/ s 3600))
          s (- s (* hours 3600))
          minutes (floor (/ s 60))
          s (- s (* minutes 60))
          seconds (floor s)
          s (- s seconds)
          millisecons (* 1000 s))
    (format "%02d:%02d:%02d,%03d" hours minutes seconds millisecons)))

(defun sese-render-subtitles ()
  "Create a new buffer with rendered subtitles in SRT format.
The new buffer file name defaults to /tmp/sese.srt,
but it won't be saved.  Use C-x C-w to save it in a different file"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (time-start
          time-end
          text
          start
          (filename (sese-get-video-file-name))
          (i 0)
          (output '()))

      ;; generate filename
      (setq filename
            (if filename
                (replace-regexp-in-string "\\.[^.]*$" ".srt" filename)
              "/tmp/sese.srt"))

      (while (re-search-forward sese-fragment-regexp nil t)

        ;; get time from regexp
        (setq time-start (string-to-number (match-string 1)))
        (setq time-end (string-to-number (match-string 4))) ; see sese-fragment-regexp

        ;; retrieve text in brackets
        (search-forward "{")
        (forward-char)
        (setq start (point))
        (re-search-forward "^}")
        (setq text (buffer-substring start (- (point) 2)))

        ;; discard tab characters
        (setq text (replace-regexp-in-string "^[ \t]+" "" text))

        ;; format and output
        (setq i (+ i 1))
        (setq time-start (sese-seconds-to-time time-start))
        (setq time-end (sese-seconds-to-time time-end))

        ;; complete text for these lines
        (setq text (format "%d\n%s --> %s\n%s"
                           i
                           time-start time-end
                           text))

        ;; append to output
        (setq output (append output `(,text))))

      ;; create and fill output buffer
      (switch-to-buffer (find-file-noselect filename))
      (erase-buffer)
      (insert (mapconcat 'identity output "\n\n"))
      (set-buffer-file-coding-system 'dos) ; for cr/lf-style newlines
      (font-lock-fontify-buffer))))

(defun sese-get-position ()
  (save-excursion
    (process-send-string sese-process "get_time_pos\n")
    (set-buffer sese-process-buffer)
    (sleep-for 0.1)
    (goto-char (point-max))
    (search-backward "=")
    (forward-char)
    (buffer-substring (point) (- (point-max) 1))))

;; mode entry function

(defun sese-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'sese-mode
        mode-name "Subtitle editor"
        indent-tabs-mode t)
  (use-local-map sese-mode-map)
  (if (string-match "Xemacs" emacs-version)
      (progn
        (make-local-variable 'font-lock-keywords)
        (setq font-lock-keywords sese-font-lock-keywords))
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults '(sese-font-lock-keywords nil t nil nil)))
  (font-lock-mode t)
  (set (make-local-variable 'sese-process-buffer) "*sese-process*")
  (set (make-local-variable 'sese-process) "sese-process")
  (set (make-local-variable 'nuke-trailing-whitespace-p) t)
  (turn-off-auto-fill)
  (setq fill-column 59)
  (filladapt-mode t)
  (run-hooks sese-mode-hook))

(provide 'sese-mode)
