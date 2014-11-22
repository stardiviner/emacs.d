;;; init-my-prog-lang-emacs-lisp.el --- init Emacs Lisp for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(require 'init-my-prog-lang-lisp)


;;; [ Emacs Lisp some setups ]

;;; custom functions:
;;;
(defun my-recompile-elc-on-save ()
  "Recompile your elc when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (file-exists-p (byte-compile-dest-file buffer-file-name))
                (emacs-lisp-byte-compile)))
            nil
            t))

;; Emacs Lisp hook
(add-hook 'emacs-lisp-hook
          (lambda ()
            (turn-on-eldoc-mode)
            (my-recompile-elc-on-save)
            (rainbow-mode +1)))


(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))



;;; [ ElDoc ]

(require 'eldoc)
(eval-after-load 'eldoc '(diminish 'eldoc-mode))


;;; eldoc-eval --- Enable eldoc support when minibuffer is in use.

(require 'eldoc-eval)


;;; elisp-format
(require 'elisp-format)

(setq elisp-format-indent-comment t
      elisp-format-dired-mark-files-confirm t)

;; FIXME:
;; (define-key emacs-lisp-mode-map (kbd "M-q")
;;   (lambda ()
;;     (interactive)
;;     (if (use-region-p)
;;         (elisp-format-region)
;;       ;; (elisp-format-buffer)
;;       )))


;;; elisp-slime-nav

;;; Usage:
;; - [M-. / M-, ] -- go to/out navigation of the function definition.
;; - [C-c C-d d] -- slime-describe-symbol.

(require 'elisp-slime-nav)
(eval-after-load 'elisp-slime-nav
  '(diminish 'elisp-slime-nav-mode))

(dolist (hook '(emacs-lisp-mode-hook
		ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))



;;; [ auto-complete-emacs-lisp ]

;; (require 'auto-complete-emacs-lisp)
;;
;; ;; this add emacs lisp source into AC, and support show popup help doc.
;; (dolist (hook '(emacs-lisp-mode-hook
;;                 eval-expression-minibuffer-setup-hook))
;;   (add-hook hook 'ac-emacs-lisp-mode-setup))


;;; [ IELM (ELISP interactive) ] -- an REPL for emacs. (Read-Eval-Print-Loop)

(require 'ielm)

;;; By default, IELM evaluates complete expressions automatically as soon you as
;;; you press Enter. So one thing to remember is that if you want to have
;;; multi-line expression (like above), you must make sure that after each line
;;; the expression is not complete (i.e., the brackets are not balanced) --
;;; otherwise the expression will be evaluated too early. That makes modes like
;;; autopair or paredit a bit inconvenient for this.
;;;
;;; If you don't like that behavior, you can do:
;;
(setq ielm-dynamic-return t)

;; TODO: (setq ielm-prompt)

(add-hook 'ielm-mode-hook
          (lambda ()
            (elisp-slime-nav-mode 1)))

;; ---------------------------------------------------------------
;;; enable auto-complete support in ielm.
;; (defun ielm-auto-complete ()
;;   "Enables `auto-complete' support in \\[ielm]."
;;   (setq ac-sources '(ac-source-functions
;;                      ac-source-variables
;;                      ac-source-symbols
;;                      ac-source-features
;;                      ac-source-words-in-same-mode-buffers))
;;   (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
;;   (auto-complete-mode 1))
;;
;; (add-hook 'ielm-mode-hook 'ielm-auto-complete)

;; or
;; (add-hook 'ielm-mode-hook
;;           (lambda ()
;;             (auto-complete-mode 1)
;;             (ac-emacs-lisp-mode-setup)))

;; ---------------------------------------------------------------

(defun my-ielm-start-or-switch ()
  "Start IELM or switch to its buffer if it already exist."
  (interactive)
  (let ((default-directory (getenv "HOME")))
    (my-func/open-and-switch-to-buffer 'ielm "*ielm*" t)))

;; (add-hook 'emacs-startup-hook 'my-ielm-start-or-switch)

(define-key my-prog-inferior-map (kbd "l e") 'my-ielm-start-or-switch)








(provide 'init-my-prog-lang-emacs-lisp)

;;; init-my-prog-lang-emacs-lisp.el ends here
