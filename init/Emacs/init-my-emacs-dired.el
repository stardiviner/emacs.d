;;; init-my-emacs-dired.el --- init Dired for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Dired ] (Directory editing mode)
;; Usage:
;; - [C-x d] -- dired (cond 'helm-mode (helm-mode-dired))
;; - [C-x C-d] -- simple entry list.
;; - [C-u C-x C-d] -- detail entry list.

(require 'dired)
(require 'dired-x)
(require 'wdired)


;; `dired-do-shell-command' does not know (by default) how to handle some
;; filetypes.
(setq dired-guess-shell-alist-user
      '(("\\.pdf\\'" (if (exec-installed-p "okular")
                         "okular"
                       "evince"))       ; PDF
        ("\\.mm\\'" (if (exec-installed-p "freeplane")
                        "freeplane"
                      "freemind"))      ; Mind Maps
        ("\\.tex\\'" "pdflatex")        ; TeX
        ("\\.ods\\'\\|\\.xlsx?\\'\\|\\.docx?\\'\\|\\.csv\\'" "libreoffice") ; Office
        ))

;; Another thing that did annoy me was the fact that when I traverse the
;; directory hierarchy, I leave a trail of open Dired buffers with all the
;; directories I go through. This is also easy to change: just enable the
;; `dired-find-alternate-file' function (bound to a in Dired) and use it to
;; visit a file or directory in place (IOW, open it instead of the current Dired
;; buffer – this also works for files I want to visit!).
;;
(put 'dired-find-alternate-file 'disabled nil) ; key [a] in Dired.


;; allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top) ; 'top means ask once

;; Dired tries to guess a default target directory.
;;
;; This means: if there is a Dired buffer displayed in the next window, use its
;; current directory, instead of this Dired buffer's current directory.
;;
(setq dired-dwim-target t)

;; How to make dired use the same buffer for viewing directory?
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

;; TODO: modify this function to my version.
(defun xah-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

Version 2015-01-26
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'"
  (interactive)
  (let* (
         (ξfile-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (ξdo-it-p (if (<= (length ξfile-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))

    (when ξdo-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (fPath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) ξfile-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (fPath) (shell-command (format "open \"%s\"" fPath)))  ξfile-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) ξfile-list))))))


;;; [ direx ] --- direx.el is a simple directory explorer. It also works as a generic tree explore library.

;;; Usage:
;;
;; - [C-c C-j]
;; - [TAB], [Enter]

(require 'direx)

;; direx-project -- (bundled with direx.el) -- project tree explorer.
(require 'direx-project)


(defun my-direx:jump-to-directory ()
  (interactive)
  (if (projectile-project-root)
      ;; (direx-project:jump-to-project-root-other-window)
      (direx-project:jump-to-project-root)
    (direx:jump-to-directory-other-window)
    ))

(global-set-key (kbd "C-c C-j") 'my-direx:jump-to-directory)

;;; put direx under popwin management.
;; (push '(direx:direx-mode :position left :width 35 :dedicated t)
;;       popwin:special-display-config)


;;; [ emacs-dired-k ] -- highlights dired buffer like "k".

;; Usage:
;;
;; - `dired-k'
;;
;; Highlight dired buffer by following parameters.
;;
;;  - File size
;;  - Modified time
;;  - Git status(if here is in git repository)

(require 'dired-k)

(setq dired-k-style 'k.zsh) ; nil, 'k.zsh, 'git
(setq dired-k-human-readable t)

(define-key dired-mode-map (kbd "K") 'dired-k)
;; always execute dired-k when dired buffer is opened
(add-hook 'dired-initial-position-hook 'dired-k)

;;; for "direx-k"
(require 'direx-k)

(define-key direx:direx-mode-map (kbd "K") 'direx-k)


;;; [ dired-efap ] -- Edit Filename At Point in an Emacs' dired buffer

;;; Usage:
;;
;; - [F2] / double clicking => rename => [RET] :: edit filename.
;; - [C-g] :: abort

(require 'dired-efap)

(setq dired-efap-use-mouse t)

;; (setq dired-efap-initial-filename-selection 'no-extension)

(set-face-attribute 'dired-efap-face nil
                    :box '(:color "orange" :line-width 2))

(define-key dired-mode-map [f2] 'dired-efap)
(define-key dired-mode-map [down-mouse-1] 'dired-efap-click)


;;; [ peep-dired ] -- A convienent way to look up file contents in other window while browsing directory in dired

;;; Usage:
;;
;; - [C-n/p] :: scroll, and auto display file content.
;; - [SPC], [C-SPC] / [backspace] :: scroll file content down/up.

(require 'peep-dired)

;; FIXME: (add-hook 'dired-mode-hook 'peep-dired)

;;; When disabling the mode you can choose to kill the buffers that were opened
;;; while browsing the directories.
(setq peep-dired-cleanup-on-disable t)
;;; Or you can choose to kill the buffer just after you move to another entry in
;;; the dired buffer.
(setq peep-dired-cleanup-eagerly t)
;; If you want the dired buffers that were peeped to have the mode enabled set
;; it to true.
(setq peep-dired-enable-on-directories nil)

;;; Evil integration
;;
;; Adjust the state name depending on an evil state you open dired in:
;;
;; (evil-define-key 'normal peep-dired-mode-map (kbd "<SPC>") 'peep-dired-scroll-page-down
;;                  (kbd "C-<SPC>") 'peep-dired-scroll-page-up
;;                  (kbd "<backspace>") 'peep-dired-scroll-page-up
;;                  (kbd "j") 'peep-dired-next-file
;;                  (kbd "k") 'peep-dired-prev-file)
;; (add-hook 'peep-dired-hook 'evil-normalize-keymaps)

;;; Ignoring Certain File Extensions
(setq peep-dired-ignored-extensions '("mkv" "iso" "mp4"))


;;; [ dired-rainbow ] -- extended file highlighting according to its type

;; This package adds more customizable highlighting for files in dired
;; listings.  The group `dired-faces' provides only nine faces and
;; isn't very fine-grained.
;;
;; The definitions are added by several macros, currently available
;; are:
;;
;; * `dired-rainbow-define` - add face by file extension
;; * `dired-rainbow-define-chmod` - add face by file permissions
;;
;; You can display their documentation by calling (substituting the
;; desired macro name):
;;
;; M-x describe-function RET dired-rainbow-define RET
;;
;; Here are some example uses:
;;
;; (defconst my-dired-media-files-extensions
;;   '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg")
;;   "Media files.")
;;
;; (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
;; (dired-rainbow-define media "#ce5c00" my-dired-media-files-extensions)
;;
;; ; boring regexp due to lack of imagination
;; (dired-rainbow-define log (:inherit default
;;                            :italic t) ".*\\.log")
;;
;; ; highlight executable files, but not directories
;; (dired-rainbow-define-chmod executable-unix "Green" "-.*x.*")
;;
;; See https://github.com/Fuco1/dired-hacks for the entire collection.


(require 'dired-rainbow)

;; executable: chmod +x
(dired-rainbow-define-chmod executable-unix "green" "-.*x.*")

;; link file
(dired-rainbow-define-chmod type-link "orange" "l.*")

;; hidden file
;; FIXME: (dired-rainbow-define hidden-files (:inherit default :italic t) "\\.*")

;; media file
(defconst my/dired-media-files-extensions
  '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg")
  "Media files.")

(dired-rainbow-define media "cyan" my/dired-media-files-extensions)

;; image file
(defconst my/dired-image-files-extensions
  '("ping" "jpg" "jpeg" "svg")
  "Image files.")

(dired-rainbow-define image "orange red" my/dired-image-files-extensions)

;; code: elisp
(defconst my/dired-source-code-emacs-lisp-extensions
  '("el" "lisp")
  "Emacs Lisp source code files.")

(dired-rainbow-define elisp "tomato" my/dired-source-code-emacs-lisp-extensions)

;; code: Ruby
(defconst my/dired-source-code-ruby-extensions
  '("rb")
  "Ruby source code files.")

(dired-rainbow-define ruby "red2" my/dired-source-code-ruby-extensions)

;; code: Python
(defconst my/dired-source-code-python-extensions
  '("py")
  "Python source code files.")

(dired-rainbow-define python "royal blue" my/dired-source-code-python-extensions)

;; code: C & C++, Go, Lua, Swift, C#, Object-C,
(defconst my/dired-source-code-c-family-extensions
  '("c" "cpp" "go" "lua")
  "C family languages source code.")

(dired-rainbow-define c-family "white smoke" my/dired-source-code-c-family-extensions)

;; code: HTML, CSS
(defconst my/dired-source-code-web-extensions
  '("html" "css")
  "Web files extensions.")

(dired-rainbow-define web-files "magenta" my/dired-source-code-web-extensions)

;; code: JavaScript, CoffeeScript,
(defconst my/dired-source-code-javascript-extensions
  '("js")
  "JavaScript source code files.")

(dired-rainbow-define js "orange" my/dired-source-code-javascript-extensions)

;; data file: xml, json,
(defconst my/dired-source-code-data-file-extensions
  '("xml" "json" "dat")
  "Data file formats.")

(dired-rainbow-define data-file "dark orange" my/dired-source-code-data-file-extensions)

;; database files:
(defconst my/dired-source-code-data-file-extensions
  '("sql")
  "Data file formats.")

(dired-rainbow-define data-file "dark magenta" my/dired-source-code-data-file-extensions)

;; config file: yaml,
(defconst my/dired-source-code-data-file-extensions
  '("yaml")
  "Data file formats.")

(dired-rainbow-define config-file "cyan3" my/dired-source-code-data-file-extensions)



;;; The function above will open the current directory in sudo mode. I decided
;;; to bind it to !, since the default & seems strictly better than !. The
;;; function will ask you for the password once. Afterwards, you can open other
;;; directories without having to enter the password.

(defun sudired ()
  "The sudo privilege to change the owner of a file owned by root."
  (interactive)
  (require 'tramp)
  (let ((dir (expand-file-name default-directory)))
    (if (string-match "^/sudo:" dir)
        (user-error "Already in sudo")
      (dired (concat "/sudo::" dir)))))

(define-key dired-mode-map "!" 'sudired)



(provide 'init-my-emacs-dired)

;;; init-my-emacs-dired.el ends here
