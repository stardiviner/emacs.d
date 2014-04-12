;;; init-my-prog-lang-common-lisp.el --- init Common Lisp for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(require 'init-my-prog-lang-lisp)


;;; [ Common Lisp ]

;; Open files with .cl extension in lisp-mode
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))




;;; [ SBCL ]

;; the SBCL configuration file is in Common Lisp
(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))



;;; [ Quick Lisp ]

;; Common Lisp support depends on SLIME being installed with Quicklisp
(if (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (message "%s" "SLIME is not installed. Use Quicklisp to install it."))


;;; [ SLIME ]

;;; Usage:
;;; - [M-x slime] ::

(load (expand-file-name "~/quicklisp/slime-helper.el"))

;; (require 'slime)
(require 'slime-autoloads)

(setq inferior-lisp-program "sbcl")

(setq slime-contribs '(slime-fancy))

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

(eval-after-load "slime"
  '(progn
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol ; 'slime-simple-complete-symbol.
           slime-fuzzy-completion-in-place t
           slime-enable-evaluate-in-emacs t
           slime-autodoc-use-multiline-p t)
     
     (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
     (define-key slime-mode-map (kbd "C-c i") 'slime-inspect)
     (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)))


(provide 'init-my-prog-lang-common-lisp)

;;; init-my-prog-lang-common-lisp.el ends here
