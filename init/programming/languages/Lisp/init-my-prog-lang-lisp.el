;;; init-my-prog-lang-lisp.el --- Lisp dialects init
;;
;;; Commentary:

;;; Code:

;;; [ Lisp ]
(setq lisp-dialects-mode-hook '(lisp-mode-hook
                                lisp-interaction-mode-hook
                                emacs-lisp-mode-hook
                                eval-expression-minibuffer-setup-hook
                                ielm-mode-hook
                                ;; common-lisp-mode-hook
                                scheme-mode-hook
                                clojure-mode-hook
                                cider-repl-mode-hook
                                ))



;;; [ Slime ] --

;; (require 'slime)


;;; [ ac-slime ] --

(dolist (hook '(lisp-mode-hook
                lisp-interaction-mode-hook
                scheme-mode-hook
                ))
  (add-hook hook (lambda ()
                   (after-load 'auto-complete
                     (add-to-list 'ac-sources 'ac-slime)))))


(set-face-attribute 'ac-slime-menu-face nil
                    :foreground "blue"
                    :bold 'normal)
;; (set-face-attribute 'ac-slime-selection-face nil
;;                     )


;;; [ Emacs Lisp ]


;;; elisp-slime-nav

;;; Usage:
;; - [M-. | M-, ] -- go to/out navigation of the function definition.
;; - [C-c C-d d] -- slime-describe-symbol.

(require 'elisp-slime-nav)

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))


;;; elisp-format
(require 'elisp-format)



(provide 'init-my-prog-lang-lisp)

;;; init-my-prog-lang-lisp.el ends here
