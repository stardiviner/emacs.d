;;; init-my-emacs-minibuffer.el --- init Emacs minibuffer

;;; Commentary:

;;; Code:

;;; [ minibuffer ]

(setq enable-recursive-minibuffers t)   ; enable to use minibuffer recursively.


;;; [ icomplete-mode ] -- incremental minibuffer completion.
;; - [C-h f icomplete-completions]
;; - (...) :: a single prospect is identified and matching is enforced,
;; - [...] :: a single prospect is identified but matching is optional,
;; - {...} - multiple prospects, separated by commas, are indicated, and further input is required to distinguish a single one.
;; Usage:
;; - [C-.] -- forward completion
;; - [C-,] -- backward completion

;; (icomplete-mode +1)



;;; find file at point (ffap)



;; eldoc-eval --- Enable eldoc support when minibuffer is in use. [M-:]

;; Eldoc info is shown by default in mode-line, but you can have eldoc info
;; somewhere else by setting eldoc-in-minibuffer-show-fn to another function
;; (e.g tooltip-show).
(setq eldoc-in-minibuffer-show-fn 'tooltip-show) ; 'eldoc-show-in-mode-line.

;; It also provides a convenient macro to enable eldoc support in your own
;; functions using minibuffer or in your defadvices, that is
;; with-eldoc-in-minibuffer, e.g:
;;
;; (defadvice edebug-eval-expression (around with-eldoc activate)
;;   "This advice enable eldoc support."
;;   (interactive (list (with-eldoc-in-minibuffer
;;                        (read-from-minibuffer
;;                         "Eval: " nil read-expression-map t
;;                         'read-expression-history))))
;;   ad-do-it)

;; Users of own minibuffer frame will have to set
;; `eldoc-in-minibuffer-own-frame-p' to non-nil.

;; You can turn On/Off eldoc support in minibuffer at any time with
;; `eldoc-in-minibuffer-mode'.

;; TODO: this eldoc-eval does not looks great.
;; (autoload 'eldoc-in-minibuffer-mode "eldoc-eval")
;; (eldoc-in-minibuffer-mode 1)





(provide 'init-my-emacs-minibuffer)

;;; init-my-emacs-minibuffer.el ends here
