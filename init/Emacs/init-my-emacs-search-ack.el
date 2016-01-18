;;; init-my-emacs-search-ack.el --- init for Ack
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Ack ]

;; (require 'ack)


;;; [ Full Ack ] -- An Emacs front-end for ack

;;; Usage:
;; - Run ack to search for all files and ack-same to search for files of the same
;;   type as the current buffer.
;; - next-error and previous-error can be used to jump to the matches.
;; - ack-find-file and ack-find-same-file use ack to list the files in the current
;;   project. It's a convenient, though slow, way of finding files.

;; (require 'full-ack)

;; (autoload 'ack-same "full-ack" nil t)
;; (autoload 'ack "full-ack" nil t)
;; (autoload 'ack-find-same-file "full-ack" nil t)
;; (autoload 'ack-find-file "full-ack" nil t)


;;; [ ack-and-a-half ]

;; (use-package ack-and-a-half
;;   :ensure t
;;   :config
;;   )

;; ;; Create shorter aliases
;; (defalias 'ack 'ack-and-a-half)
;; (defalias 'ack-same 'ack-and-a-half-same)
;; (defalias 'ack-find-file 'ack-and-a-half-find-file)
;; (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
;;
;; (setq ack-and-a-half-use-ido t               ; use ido to provide completions
;;       ;; ack-and-a-half-executable "ack-grep"
;;       ;; ack-and-a-half-arguments ; extra arguments passed to ack
;;       ack-and-a-half-ignore-case 'smart
;;       ack-and-a-half-regexp-search t
;;       ack-and-a-half-regexp-history t
;;       ack-and-a-half-use-environment t
;;       ;; (ack-and-a-half-same)
;;       ;; ack-and-a-half-mode-type-default-alist
;;       ack-and-a-half-mode-type-alist nil
;;       ack-and-a-half-literal-history t
;;       ;; ack-and-a-half-prompt-for-directory 'unless-guessed
;;       ;; ack-and-a-half-root-directory-functions '(ack-and-a-half-guess-project-root)
;;       ack-and-a-half-prompt-for-directory t
;;       )
;;
;; ;; add more project root file patterns.
;; ;; (add-to-list 'ack-and-a-half-project-root-file-patterns "\\.kk")
;;
;; (unless (boundp 'ack-map)
;;   (define-prefix-command 'ack-map))
;; (define-key my-search-prefix (kbd "k") 'ack-map)
;;
;; (define-key ack-map (kbd "k") 'ack)
;; (define-key ack-map (kbd "s") 'ack-same)
;; (define-key ack-map (kbd "f") 'ack-find-file)
;; (define-key ack-map (kbd "F") 'ack-find-file-same)



(provide 'init-my-emacs-search-ack)

;;; init-my-emacs-search-ack.el ends here
