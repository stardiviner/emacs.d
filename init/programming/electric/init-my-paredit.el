;;; init-my-paredit.el --- init ParEdit
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Par Edit (paredit) ] -- performing structured editing of S-expression data.

(require 'paredit)
;; (autoload 'paredit-mode "paredit" "minor mode for pseudo-structurally editing Lisp code." t)

(eval-after-load "paredit"
  '(progn
     ;; remove following keybindings
     (dolist (binding (list (kbd "C-<left>") (kbd "C-<right>")
                            (kbd "C-M-<left>") (kbd "C-M-<right>")))
       (define-key paredit-mode-map binding nil))
     ;; disable kill-sentence, which is easily confused with the kill-sexp binding, but doesn't
     ;; preserve sexp structure
     (define-key paredit-mode-map [remap kill-sentence] nil)
     (define-key paredit-mode-map [remap backward-kill-sentence] nil)))

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
