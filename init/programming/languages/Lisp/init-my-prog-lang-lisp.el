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



;;; [ SLIME ]

;;; Usage:
;;; - [M-x slime] ::

;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (require 'slime)

;; NOTE: currently using slime from el-get installation.

;;; -----------------------------------
(require 'slime-autoloads)

(setq inferior-lisp-program "sbcl")
;; (setq slime-contribs '(slime-fancy))
;;; -----------------------------------

;; a list of alternative Common Lisp implementations that can be
;; used with SLIME. Note that their presence render
;; inferior-lisp-program useless. This variable holds a list of
;; programs and if you invoke SLIME with a negative prefix
;; argument, M-- M-x slime, you can select a program from that list.
(setq slime-lisp-implementations
      '((ccl ("ccl"))
        (clisp ("clisp" "-q"))
        (cmucl ("cmucl" "-quiet"))
        (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))

;; select the default value from slime-lisp-implementations
(if (eq system-type 'darwin)
    ;; default to Clozure CL on OS X
    (setq slime-default-lisp 'ccl)
  ;; default to SBCL on Linux and Windows
  (setq slime-default-lisp 'sbcl))

(defun my-start-slime ()
  "Start SLIME unless it's already running."
  (unless (slime-connected-p)
    (save-excursion (slime))))

;; start slime automatically when we open a lisp file
(add-hook 'slime-mode-hook 'my-start-slime)

(eval-after-load 'slime
  '(progn
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol ; 'slime-simple-complete-symbol.
           slime-fuzzy-completion-in-place t
           slime-enable-evaluate-in-emacs t
           slime-autodoc-use-multiline-p t)
     
     (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
     (define-key slime-mode-map (kbd "C-c i") 'slime-inspect)
     (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)))



;;; [ ac-slime ] --

;; (require 'ac-slime)
;;
;; (dolist (hook '(lisp-mode-hook
;;                 lisp-interaction-mode-hook
;;                 scheme-mode-hook
;;                 ))
;;   (add-hook hook (lambda ()
;;                    (add-to-list 'ac-sources 'ac-slime))))
;;
;;
;; (set-face-attribute 'ac-slime-menu-face nil
;;                     :foreground "yellow"
;;                     :bold 'normal)
;; (set-face-attribute 'ac-slime-selection-face nil
;;                     )


;;; [ slime-company ] -- slime backend for Company mode.


;; A quick way to jump to the definition of a function given its key binding
;; (global-set-key (kbd "C-h K") 'find-function-on-key)



;;; [ Swank ] (cl-swank) --





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

;; (run-geiser)


;;; [ ac-geiser ]

;; (require 'ac-geiser)
;;
;; (dolist (hook '(lisp-mode-hook
;;                 lisp-interaction-mode-hook
;;                 scheme-mode-hook
;;                 ))
;;   (add-hook hook (lambda ()
;;                    (ac-geiser-setup)
;;                    (add-to-list 'ac-sources 'ac-source-geiser))))


;;; [ Quack ] -- enhanced Emacs Support for Editing and Running Scheme Code

;;; http://www.neilvandyke.org/quack/


(provide 'init-my-prog-lang-lisp)

;;; init-my-prog-lang-lisp.el ends here
