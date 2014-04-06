;;; init-my-prog-electric.el --- init electric stuff.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; Rainbow Mode (rainbow-mode.el -- colorize color names in buffers)

(require 'rainbow-mode)
(dolist (hook
         '(emacs-lisp-mode-hook
           css-mode-hook
           html-mode-hook))
  (add-hook hook (lambda () (rainbow-mode 1))))


;;; Rainbow Delimiters

(when (require 'rainbow-delimiters nil 'noerror)
  (rainbow-delimiters-mode t)
  ;; (global-rainbow-delimiters-mode) ; global
  ;; enable in all programming-related modes
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  ;; enable in specific modes
  ;; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  )


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

;;; FIXME how to only enable paredit in those programming language modes.
(dolist (hook
         '(;; Lisp dialects
           emacs-lisp-mode-hook
           eval-expression-minibuffer-setup-hook
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


;;; [ autopair ]

(require 'autopair)

(autoload 'autopair "autopair" t)

(setq autopair-pair-criteria 'help-balance
      autopair-skip-criteria 'help-balance
      autopair-autowrap 'help-balance ; 'help-balance, t
      autopair-blink t)

(setq autopair-extra-pairs `(:everywhere ((?‘. ?’)
                                          (?“. ?”)
                                          (?（. ?）)
                                          (?【. ?】)
                                          (?〖. ?〗)
                                          (?『. ?』))))


;; (autopair-global-mode 1)
(dolist (hook
         '(org-mode-hook
           ;; programming modes
           prog-mode-hook
           ;; ess-mode-hook                ; Emacs Speaks Statistics
           ))
  (add-hook hook #'(lambda ()
                     ;; FIXME test whether paredit is active
                     ;; (memq 'paredit-mode minor-mode-list)
                     (unless (and (boundp 'paredit-mode) paredit-mode)
                       (autopair-mode)))))



;;; More tricks
;;; prevent the { (opening brace) character from being autopaired in C++ comments.
;; (add-hook 'c++-mode-hook
;;           #'(lambda ()
;;               (push ?{
;;                     (getf autopair-dont-pair :comment))))
;;; autopair-handle-action-fns lets you write some emacs-lisp that overrides/extends the actions taken by autopair after it decides something must be paired, skipped or deleted. To work with triple quoting in python mode, you can use this for example:
;; (add-hook 'python-mode-hook
;;           #'(lambda ()
;;               (setq autopair-handle-action-fns
;;                     (list #'autopair-default-handle-action
;;                           #'autopair-python-triple-quote-action))))
;;; autopair-extra-pairs lets you define extra pairing and skipping behaviour for pairs not programmed into the syntax table. Watch out, this is work-in-progress, a little unstable and does not help balancing at all. To have < and > pair in c++-mode buffers, but only in code, use:
;; (add-hook 'c++-mode-hook
;;           #'(lambda ()
;;               (push '(?< . ?>)
;;                     (getf autopair-extra-pairs :code))))
;;; if you program in emacs-lisp you might also like the following to pair backtick (`) and quote (=’=).
;;; for quote Emacs Lisp code. e.g. `org-mode'
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (setq autopair-extra-pairs `(:comment ((?`. ?'))))
              ;; (push '(?` . ?')
              ;;       (getf autopair-extra-pairs :comment))
              ;; (push '(?` . ?')
              ;;       (getf autopair-extra-pairs :string))
              )
          )



(provide 'init-my-prog-electric)

;;; init-my-prog-electric.el ends here
