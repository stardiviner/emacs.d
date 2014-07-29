;;; init-my-paredit.el --- init ParEdit
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; Par Edit (paredit) -- a minor mode for performing structured editing of S-expression data.

;; Usage:
;; - Basic Insertion Commands
;;   (), [M-)], [], "", [M-"], \, ;, [M-;], [C-j],
;; - Deleting & Killing
;;   [C-d], DEL, [C-k], [M-DEL],
;; - Movement & Navigation
;;   [C-M-f], [C-M-b] -- paredit-(forward/backward)
;;   [C-M-u], [C-M-d] -- paredit-(backward-up/forward-down)
;;   [C-M-p], [C-M-n] -- paredit-(backward-down/forward-up)
;; - Depth-Changing Commands
;;   [M-(] -- paredit-wrap-around
;;   [M-s] -- paredit-splice-sexp
;;   [M-<up>] -- paredit-splice-sexp-killing-backward
;;   [M-<down>] -- paredit-splice-sexp-killing-forward
;;   [M-r] -- paredit-raise-sexp
;; - Barfage & Slurpage
;;   [C-)] -- paredit-forward-slurp-sexp
;;   [C-}] -- paredit-forward-barf-sexp
;;   [C-(] -- paredit-backward-slurp-sexp
;;   [C-{] -- paredit-backward-barf-sexp
;; - Miscellaneous Commands
;;   [M-S] -- paredit-split-sexp
;;   [M-J] -- paredit-join-sexps
;;   [C-c C-M-l] -- paredit-recentre-on-sexp
;;   [M-q] -- paredit-reindent-defun

(require 'paredit)
;; (autoload 'paredit-mode "paredit" "minor mode for pseudo-structurally editing Lisp code." t)

(after 'paredit
  (diminish 'paredit-mode " Par")
  (dolist (binding (list (kbd "C-<left>") (kbd "C-<right>")
                         (kbd "C-M-<left>") (kbd "C-M-<right>")))
    (define-key paredit-mode-map binding nil))
  ;; disable kill-sentence, which is easily confused with the kill-sexp binding, but doesn't
  ;; preserve sexp structure
  (define-key paredit-mode-map [remap kill-sentence] nil)
  (define-key paredit-mode-map [remap backward-kill-sentence] nil))

;;; enable some handy paredit functions in all prog modes
;; (add-hook 'prog-mode-hook 'paredit-everywhere-mode)

;;; Only enable paredit in those programming language modes.
(dolist (hook
         '(;; Lisp dialects
           emacs-lisp-mode-hook
           eval-expression-minibuffer-setup-hook
           ;; inferior-emacs-lisp-mode-hook
           ielm-mode-hook
           lisp-mode-hook
           lisp-interaction-mode-hook
           scheme-mode-hook
           clojure-mode-hook
           cider-repl-mode-hook
           ;; NOTICE do not enable paredit-mode in all programming modes.
           ;; prog-mode-hook               ; programming languages source code mode hook.
           ))
  (add-hook hook 'paredit-mode))

;;; ParEdit inside mini-buffer
;;
;; use paredit in the minibuffer
;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
  "Interactive commands for which paredit should be enabled in the minibuffer.")
(defun conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)



(provide 'init-my-paredit)

;;; init-my-paredit.el ends here
