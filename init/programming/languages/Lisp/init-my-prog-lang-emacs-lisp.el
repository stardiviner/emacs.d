;;; init-my-prog-lang-emacs-lisp.el --- init Emacs Lisp for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Emacs Lisp some setups ]

(setq print-quoted t
      print-circle t)


;; Emacs Lisp hook
(add-hook 'emacs-lisp-mode-hook #'my-lisp-common-settings)

(add-hook 'inferior-emacs-lisp-mode-hook #'my-lisp-repl-common-settings)

(defun my-emacs-lisp-setup ()
  (interactive)
  ;; company-elisp
  (my-company-add-backend-locally 'company-elisp)
  (setq company-elisp-detect-function-context t
        company-elisp-show-locals-first t
        )
  )

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-setup)

;; Recompile your elc when saving an elisp file.
(add-hook 'after-save-hook
          (lambda ()
            (when (file-exists-p (byte-compile-dest-file buffer-file-name))
              (emacs-lisp-byte-compile)))
          nil
          t)


;;; - `eval-expression-minibuffer-setup-hook'
;;
;; (setq eval-expression-debug-on-error t
;;       eval-expression-print-level nil ; 4, nil,
;;       eval-expression-print-length nil
;;       )

;;; eldoc-eval --- Enable eldoc support when minibuffer is in use.

(use-package eldoc-eval
  :ensure t
  :config
  (eldoc-in-minibuffer-mode 1)
  )


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
              (my-company-add-backend-locally 'company-elisp)
              ))
  )

;;; [ eros ] -- Evaluation Result OverlayS for Emacs Lisp.

(use-package eros
  :ensure t
  :config
  (eros-mode 1)
  (set-face-attribute 'eros-result-overlay-face nil
                      :foreground "orange"
                      :background (color-darken-name (face-background 'default) 5)
                      :box nil :slant 'italic
                      )
  )


;;; [ macrostep ] -- interactive macro-expander for Emacs.

(use-package macrostep
  :ensure t
  :bind (:map emacs-debug-prefix
              ("m" . macrostep-expand))
  :config
  (setq macrostep-expand-in-separate-buffer nil
        macrostep-expand-compiler-macros t)

  ;; macro expansion background highlight color
  (set-face-attribute 'macrostep-expansion-highlight-face nil
                      :inherit nil
                      :background "#222222" :foreground nil
                      )
  ;; macro
  (set-face-attribute 'macrostep-macro-face nil
                      :inherit nil
                      :foreground "cyan"
                      :background (color-darken-name (face-background 'default) 5)
                      :bold t :underline nil :overline t)
  ;; compiler macro
  (set-face-attribute 'macrostep-compiler-macro-face nil
                      :inherit nil
                      :foreground "gray"
                      :background (color-darken-name (face-background 'default) 5)
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
  :defer t
  :config
  (setq elmacro-concatenate-multiple-inserts t
        elmacro-objects-to-convert '(frame window buffer)
        ;; elmacro-unwanted-commands-regexp "^\\(ido\\|smex\\)"
        ;; elmacro-additional-recorded-functions
        ;; '(copy-file copy-directory rename-file delete-file make-directory)
        )
  )


;;; thread and unwind code in Emacs Lisp

;;; TODO: integrate into `emr'.

;; (use-package clj-refactor
;;   :ensure t
;;   :defer t)
;;
;; (defun my/elisp-thread-last ()
;;   "Turn the form at point into a `thread-last' form."
;;   (interactive)
;;   (cljr-thread-last-all nil)
;;   (save-excursion
;;     (when (search-backward "->>" nil 'noerror)
;;       (replace-match "thread-last"))))
;;
;; (defun my/elisp-thread-first ()
;;   "Turn the form at point into a `thread-first' form."
;;   (interactive)
;;   (cljr-thread-first-all nil)
;;   (save-excursion
;;     (when (search-backward "->" nil 'noerror)
;;       (replace-match "thread-first"))))
;;
;; (defun my/elisp-unwind ()
;;   "Unwind thread at point or above point by one level.
;; Return nil if there are no more levels to unwind."
;;   (interactive)
;;   (let ((p (point)))
;;     ;; Find a thread above.
;;     (when (save-excursion
;;             (forward-sexp 1)
;;             (and (search-backward-regexp "\\_<thread-\\(first\\|last\\)\\_>" nil 'noerror)
;;                  ;; Ensure that it contains the original point.
;;                  (save-match-data (forward-char -1)
;;                                   (forward-sexp 1)
;;                                   (> (point) p))))
;;       (replace-match (if (string= (match-string 1) "first")
;;                          "->" "->>"))
;;       (let ((thread-beginnig (match-beginning 0)))
;;         (prog1 (cljr-unwind)
;;           (save-excursion
;;             (goto-char thread-beginnig)
;;             (when (looking-at "\\_<->>?\\_>")
;;               (replace-match (if (string= (match-string 0) "->")
;;                                  "thread-first" "thread-last")))))))))
;;
;; (defun my/elisp-unwind-all ()
;;   "Fully unwind thread at point or above point."
;;   (interactive)
;;   (while (my/elisp-unwind)))
;;
;; (define-key emacs-lisp-mode-map (kbd "C-c t f") #'my/elisp-thread-first)
;; (define-key emacs-lisp-mode-map (kbd "C-c t l") #'my/elisp-thread-last)
;; (define-key emacs-lisp-mode-map (kbd "C-c t u") #'my/elisp-unwind)
;; (define-key emacs-lisp-mode-map (kbd "C-c t a") #'my/elisp-unwind-all)


;;; [ elisp-refs ] -- semantic code search for emacs lisp.

(use-package elisp-refs
  :ensure t
  :init
  (defun elisp-refs-keybindings-setup ()
    (interactive)
    (unless (boundp 'my-prog-lookup-map)
      (define-prefix-command 'my-prog-lookup-map))
    (local-set-key (kbd "C-c l") 'my-prog-lookup-map)

    (define-key my-prog-lookup-map (kbd "s") 'elisp-refs-symbol)
    (define-key my-prog-lookup-map (kbd "f") 'elisp-refs-function)
    (define-key my-prog-lookup-map (kbd "m") 'elisp-refs-macro)
    (define-key my-prog-lookup-map (kbd "v") 'elisp-refs-variable)
    (define-key my-prog-lookup-map (kbd "S") 'elisp-refs-special)
    )
  
  (add-hook 'emacs-lisp-mode-hook #'elisp-refs-keybindings-setup)
  )


;;; [ suggest ] -- suggest elisp functions that give the output requested.

(use-package suggest
  :ensure t
  :defer t)


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
