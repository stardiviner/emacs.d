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


;;; [ direx ] --- direx.el is a simple directory explorer. It also works as a generic tree explore library.

;; (require 'direx)
;;
;; ;; (global-set-key (kbd "C-c C-j") 'direx:jump-to-directory)
;;
;; (push '(direx:direx-mode :position left :width 35 :dedicated t)
;;       popwin:special-display-config)
;; (global-set-key (kbd "C-c C-j") 'direx:jump-to-directory-other-window)
;;
;; ;;; [ direx-project ] --- (bundled with direx.el) -- project tree explorer.
;; (require 'direx-project)
;;
;; ;; (global-set-key (kbd "C-c C-p") 'direx-project:jump-to-project-root)


;;; [ emacs-dired-k ] -- highlights dired buffer like "k".

;; Usage:
;; - `dired-k'
;;
;; Highlight dired buffer by following parameters.
;;
;;     File size
;;     Modified time
;;     Git status(if here is in git repository)

;; (require 'dired-k)

;; (define-key dired-mode-map (kbd "K") 'dired-k)
;; ;; You can use dired-k alternative to revert-buffer
;; (define-key dired-mode-map (kbd "g") 'dired-k)
;; ;; always execute dired-k when dired buffer is opened
;; (add-hook 'dired-initial-position-hook 'dired-k)


;;; [ peep-dired ] -- A convienent way to look up file contents in other window while browsing directory in dired

;;; Usage:
;;
;; - [C-n/p] :: scroll, and auto display file content.
;; - [SPC], [C-SPC] / [backspace] :: scroll file content down/up.

(require 'peep-dired)

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

(provide 'init-my-emacs-dired)

;;; init-my-emacs-dired.el ends here
