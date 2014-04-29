;;; init-my-emacs-regexp.el --- init my Emacs regexp expression


;;; Commentary:

;;; [ regular expression ]


;;; Code:

(define-prefix-command 'my-regexp-prefix-map)
(global-set-key (kbd "C-c r") 'my-regexp-prefix-map)

;; (define-key my-regexp-keybinding-map (kbd "v r") 'vr/replace)


;;; [ re-builder ] -- instant regexp builder
;;; Usage:
;; - [M-x regexp-builder] -- start regexp-builder
;; - [C-c C-q] -- quit regexp-builder window.
(require 're-builder)
(autoload 're-builder "re-builder" t)

(define-key my-regexp-prefix-map (kbd "b") 're-builder)


;;; [ visual-regexp ] -- visual-regexp for Emacs is like `replace-regexp' (or `query-replace-regexp'), but with live visual feedback directly in the buffer.

(require 'visual-regexp)
;;; [ visual-regexp-steroids.el ] -- Extends visual-regexp to support other regexp engines.
(require 'visual-regexp-steroids)

(define-key my-regexp-prefix-map (kbd "v s") 'vr/isearch-forward)
(define-key my-regexp-prefix-map (kbd "v b") 'vr/isearch-backward)
(define-key my-regexp-prefix-map (kbd "v r") 'vr/replace)
(define-key my-regexp-prefix-map (kbd "v q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(if (featurep 'multiple-cursors)
    (define-key my-regexp-prefix-map (kbd "v m") 'vr/mc-mark))
;; TODO: `vr/select-mc-mark', `vr/select-replace' etc.

;;; integrate with Helm version regexp
(if (featurep 'helm)
    (define-key my-regexp-prefix-map (kbd "h") 'helm-regexp))


(provide 'init-my-emacs-regexp)

;;; init-my-emacs-regexp.el ends here





