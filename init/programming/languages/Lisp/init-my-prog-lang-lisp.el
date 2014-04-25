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


;;; [ Slime Mode ]




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


;;; [ slime-company ] -- slime backend for Company mode.


;; A quick way to jump to the definition of a function given its key binding
;; (global-set-key (kbd "C-h K") 'find-function-on-key)


;;; [ Geiser ] -- Geiser is a collection of Emacs major and minor modes that conspire with one or more Scheme interpreters to keep the Lisp Machine Spirit alive.

;;; Geiser is a collection of Emacs major and minor modes that conspire with one
;;; or more Scheme interpreters to keep the Lisp Machine Spirit alive. It draws
;;; inspiration (and a bit more) from environments such as Common Lisp’s Slime,
;;; Factor’s FUEL, Squeak or Emacs itself, and does its best to make Scheme
;;; hacking inside Emacs (even more) fun.

;;; Or, to be precise, what i consider fun. Geiser is thus my humble
;;; contribution to the dynamic school of expression, and a reaction against
;;; what i perceive as a derailment, in modern times, of standard Scheme towards
;;; the static camp. Because i prefer growing and healing to poking at corpses,
;;; the continuously running Scheme interpreter takes the center of the stage in
;;; Geiser. A bundle of Elisp shims orchestrates the dialog between the Scheme
;;; interpreter, Emacs and, ultimately, the schemer, giving her access to live
;;; metadata. Here’s how.

(require 'geiser-install)



;;; [ ac-geiser ]

(require 'ac-geiser)



(provide 'init-my-prog-lang-lisp)

;;; init-my-prog-lang-lisp.el ends here
