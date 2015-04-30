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


(provide 'init-my-emacs-dired)

;;; init-my-emacs-dired.el ends here
