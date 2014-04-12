;;; init-my-prog-lang-lisp.el --- Lisp dialects init
;;
;;; Commentary:

;;; Code:

;;; [ Lisp ]
(setq lisp-dialects-mode-hook '(lisp-mode-hook
                                lisp-interaction-mode-hook
                                ;; common-lisp-mode-hook
                                scheme-mode-hook
                                ;; clojure-mode-hook
                                cider-repl-mode-hook
                                ))



;;; [ Slime ] --

;; (require 'slime)


;;; [ ac-slime ] --

(require 'ac-slime)

(dolist (hook '(lisp-mode-hook
                lisp-interaction-mode-hook
                scheme-mode-hook
                ))
  (add-hook hook (lambda ()
                   (add-to-list 'ac-sources 'ac-slime))))


(set-face-attribute 'ac-slime-menu-face nil
                    :foreground "yellow"
                    :bold 'normal)
;; (set-face-attribute 'ac-slime-selection-face nil
;;                     )



;; A quick way to jump to the definition of a function given its key binding
;; (global-set-key (kbd "C-h K") 'find-function-on-key)



(provide 'init-my-prog-lang-lisp)

;;; init-my-prog-lang-lisp.el ends here
