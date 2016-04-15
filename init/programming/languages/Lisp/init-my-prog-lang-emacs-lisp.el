;;; init-my-prog-lang-emacs-lisp.el --- init Emacs Lisp for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Emacs Lisp some setups ]

(setq print-quoted t
      print-circle t)

;; - `eval-expression-minibuffer-setup-hook'

;; (setq eval-expression-debug-on-error t
;;       eval-expression-print-level nil ; 4, nil,
;;       eval-expression-print-length nil
;;       )

;;; custom functions:
;;;
(defun my-recompile-elc-on-save ()
  "Recompile your elc when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (file-exists-p (byte-compile-dest-file buffer-file-name))
                (emacs-lisp-byte-compile)))
            nil
            t))

;; Emacs Lisp hook
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (rainbow-mode 1)
            (turn-on-eldoc-mode)
            
            (my-recompile-elc-on-save)

            ;; company-elisp
            (my-company-add-backend-locally 'company-elisp)
            (setq company-elisp-detect-function-context t
                  company-elisp-show-locals-first t
                  )
            ))

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))


;; Emacs Lisp highlights

;; `let-alist' . symbols
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\_<\\.\\(?:\\sw\\|\\s_\\)+\\_>" 0
    font-lock-builtin-face)))


;;; eldoc-eval --- Enable eldoc support when minibuffer is in use.

(use-package eldoc-eval
  :ensure t
  :config
  (eldoc-in-minibuffer-mode 1)
  )


;;; elisp-slime-nav

;;; Usage:
;; - [M-. / M-, ] -- go to/out navigation of the function definition.
;; - [C-c C-d d] -- slime-describe-symbol.

(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook
                  ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode))
  )


;;; [ IELM (ELISP interactive) ] -- an REPL for emacs. (Read-Eval-Print-Loop)

(setq ielm-dynamic-return t)

(add-hook 'ielm-mode-hook
          (lambda ()
            (elisp-slime-nav-mode 1)
            (my-company-add-backend-locally 'company-elisp)
            ))

(unless (boundp 'my-prog-inferior-map)
  (define-prefix-command 'my-prog-inferior-map))
(global-set-key (kbd "C-c i") 'my-prog-inferior-map)

(unless (boundp 'my-inferior-lisp-map)
  (define-prefix-command 'my-inferior-lisp-map))
(define-key my-prog-inferior-map (kbd "l") 'my-inferior-lisp-map)

(defun my-ielm-start-or-switch ()
  "Start IELM or switch to its buffer if it already exist."
  (interactive)
  (let ((default-directory (getenv "HOME")))
    (my-func/open-and-switch-to-buffer 'ielm "*ielm*" t)))

(define-key my-inferior-lisp-map (kbd "e") 'my-ielm-start-or-switch)

(defun my-scratch-start-or-switch ()
  "Start IELM or switch to its buffer if it already exist."
  (interactive)
  ;; (switch-to-buffer "*scratch*")
  (popwin:display-buffer "*scratch*")
  )

(define-key my-inferior-lisp-map (kbd "k") 'my-scratch-start-or-switch)


;;; eval-result-overlays in Emacs-Lisp.

;; Assuming you already have `CIDER' installed, porting this feature to Elisp is
;; almost trivial. We just define a small wrapper around the function that
;; creates the overlay, and then advise the relevant elisp commands to call it.

(autoload 'cider--make-result-overlay "cider-overlays")

(defun my-eval-sexp-overlay (value point)
  (cider--make-result-overlay (format "%S" value)
                              :where point
                              :duration 'command)
  ;; Preserve the return value.
  value)

(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (my-eval-sexp-overlay
               (apply f beg end r)
               end)))

(advice-add 'eval-last-sexp :filter-return
            (lambda (r)
              (my-eval-sexp-overlay r (point))))

(advice-add 'eval-defun :filter-return
            (lambda (r)
              (my-eval-sexp-overlay
               r
               (save-excursion
                 (end-of-defun)
                 (point)))))


;;; [ macrostep ] -- interactive macro-expander for Emacs.

;;; Usage:
;;
;; - `macrostep-mode' minor-mode.
;; - `macrostep-expand' interactive command.
;; - [q] exit

(use-package macrostep
  :ensure t
  :config
  (setq macrostep-expand-in-separate-buffer nil
        macrostep-expand-compiler-macros t)

  (define-key my-prog-debug-map (kbd "m") 'macrostep-expand)
  (define-key my-prog-debug-map (kbd "e") 'macrostep-expand)

  ;; macro expansion background highlight color
  (set-face-attribute 'macrostep-expansion-highlight-face nil
                      :inherit nil
                      :background "#222222" :foreground nil
                      )
  ;; macro
  (set-face-attribute 'macrostep-macro-face nil
                      :inherit nil
                      :foreground "cyan" :background "#222222"
                      :bold t :overline t :underline nil)
  ;; compiler macro
  (set-face-attribute 'macrostep-compiler-macro-face nil
                      :inherit nil
                      :foreground "black" :background "dim gray"
                      :bold t :italic t :overline t)
  ;; gensym
  (set-face-attribute 'macrostep-gensym-1 nil
                      :foreground "#8080c0" :box t :bold t)
  (set-face-attribute 'macrostep-gensym-2 nil
                      :foreground "#8fbc8f" :box t :bold t)
  (set-face-attribute 'macrostep-gensym-3 nil
                      :foreground "#daa520" :box t :bold t)
  (set-face-attribute 'macrostep-gensym-4 nil
                      :foreground "#cd5c5c" :box t :bold t)
  (set-face-attribute 'macrostep-gensym-5 nil
                      :foreground "#da70d6" :box t :bold t)
  )


;;; [ elmacro ] -- display keyboard macros or latest interactive commands as emacs lisp.

(use-package elmacro
  :ensure t
  :config
  (setq elmacro-concatenate-multiple-inserts t
        elmacro-objects-to-convert '(frame window buffer)
        ;; elmacro-unwanted-commands-regexp "^\\(ido\\|smex\\)"
        ;; elmacro-additional-recorded-functions
        ;; '(copy-file copy-directory rename-file delete-file make-directory)
        )
  )


;;; [ ERT ] -- Emacs Lisp Regression Testing.

;;; Usage:
;;
;; - `ert-deftest'
;; - `ert-run-tests-batch'
;; - `ert-run-tests-batch-and-exit'
;; - `ert-run-tests-interactively' => alias `ert'
;; - `ert-describe-test'


;;; [ xtest ] -- Simple Testing with Emacs & ERT


;;; [ faceup ] -- Regression test system for font-lock


;;; [ test-simple ] -- Simple Unit Test Framework for Emacs Lisp


;;; [ buttercup ] -- Behavior-Driven Emacs Lisp Testing


(provide 'init-my-prog-lang-emacs-lisp)

;;; init-my-prog-lang-emacs-lisp.el ends here
