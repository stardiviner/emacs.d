;;; init-my-prog-lang-emacs-lisp.el --- init Emacs Lisp for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(add-hook 'emacs-lisp-mode-hook #'my-lisp-common-settings)
(add-hook 'inferior-emacs-lisp-mode-hook #'my-lisp-repl-common-settings)

(defun my-emacs-lisp-setup ()
  "My Emacs Lisp mode settings."
  (interactive)
  ;; company-elisp
  (my-company-add-backend-locally 'company-elisp)
  (setq company-elisp-detect-function-context t
        company-elisp-show-locals-first t))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-setup)

;; Recompile your elc when saving an elisp file.
(add-hook 'after-save-hook
          (lambda ()
            (when (file-exists-p (byte-compile-dest-file buffer-file-name))
              (emacs-lisp-byte-compile)))
          'append 'local)


;;; [ ob-emacs-lisp ]

(require 'ob-emacs-lisp)

(add-to-list 'org-babel-default-header-args:emacs-lisp
             '(:results . "value"))
(add-to-list 'org-babel-default-header-args:emacs-lisp
             '(:noweb . "yes"))
;; (add-to-list 'org-babel-default-header-args:emacs-lisp
;;              '(:lexical . "yes"))


;;; [ eldoc-eval ] -- Enable eldoc support when minibuffer is in use.

(use-package eldoc-eval
  :ensure t
  :config
  (eldoc-in-minibuffer-mode 1))


;;; [ IELM (ELISP interactive) ] -- an REPL for emacs. (Read-Eval-Print-Loop)

(use-package ielm
  :ensure t
  :defer t
  :config
  (setq ielm-dynamic-return t
        ielm-dynamic-multiline-inputs t)
  (add-hook 'ielm-mode-hook #'my-lisp-repl-common-settings)
  (add-hook 'ielm-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'company-elisp)))
  )

;;; [ eros ] -- Evaluation Result OverlayS for Emacs Lisp.

(use-package eros
  :ensure t
  :config
  (eros-mode 1))


;;; [ macrostep ] -- interactive macro-expander for Emacs.

(use-package macrostep
  :ensure t
  :bind (:map emacs-debug-prefix
              ("m" . macrostep-expand))
  :config
  (setq macrostep-expand-in-separate-buffer nil
        macrostep-expand-compiler-macros t))


;;; [ elmacro ] -- display keyboard macros or latest interactive commands as emacs lisp.

(use-package elmacro
  :ensure t
  :defer t
  :config
  (setq elmacro-concatenate-multiple-inserts t
        elmacro-objects-to-convert '(frame window buffer)
        ;; elmacro-unwanted-commands-regexp "^\\(ido\\|smex\\)"
        ;; elmacro-additional-recorded-functions
        ;; '(copy-file copy-directory rename-file delete-file make-directory)
        )
  )


;;; [ elisp-refs ] -- semantic code search for emacs lisp.

(use-package elisp-refs
  :ensure t
  :config
  (defun elisp-refs-keybindings-setup ()
    (interactive)
    (unless (boundp 'tags-prefix)
      (define-prefix-command 'tags-prefix))
    (local-set-key (kbd "M-g t") 'tags-prefix)
    (define-key tags-prefix (kbd "s") 'elisp-refs-symbol)
    (define-key tags-prefix (kbd "f") 'elisp-refs-function)
    (define-key tags-prefix (kbd "m") 'elisp-refs-macro)
    (define-key tags-prefix (kbd "v") 'elisp-refs-variable)
    (define-key tags-prefix (kbd "S") 'elisp-refs-special))
  (add-hook 'emacs-lisp-mode-hook #'elisp-refs-keybindings-setup)
  )


;;; [ suggest ] -- suggest elisp functions that give the output requested.

(use-package suggest
  :ensure t
  :defer t
  :commands (suggest))


;;; [ ERT ] -- Emacs Lisp Regression Testing.

;;; [ xtest ] -- Simple Testing with Emacs & ERT

;;; [ faceup ] -- Regression test system for font-lock

;;; [ test-simple ] -- Simple Unit Test Framework for Emacs Lisp

;;; [ buttercup ] -- Behavior-Driven Emacs Lisp Testing

;;; [ dash.el ] -- A modern list library for Emacs.

(use-package dash
  :ensure t
  :config
  ;; syntax highlighting of dash functions.
  (eval-after-load 'dash '(dash-enable-font-lock))
  )


(provide 'init-my-prog-lang-emacs-lisp)

;;; init-my-prog-lang-emacs-lisp.el ends here
