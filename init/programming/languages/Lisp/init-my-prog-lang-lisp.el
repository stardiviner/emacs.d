;;; init-my-prog-lang-lisp.el --- Lisp dialects init
;;
;;; Commentary:

;;; Code:

;;; [ Lisp ]

;;; Common Settings for All Lisp dialects
(use-package paredit
  :ensure t)
(use-package smartparens
  :ensure t)
(use-package rainbow-delimiters
  :ensure t)
(use-package hl-sexp
  :ensure t)

(defun my-lisp-common-settings ()
  "Common settings for all Lisp dialects."
  (interactive)
  (rainbow-delimiters-mode 1)
  (paredit-mode 1)
  ;; (smartparens-strict-mode 1)
  (hl-sexp-mode 1)
  (eldoc-mode 1)
  )

(defun my-lisp-repl-common-settings ()
  "Common settings for all Lisp dialects REPL."
  (interactive)
  (rainbow-delimiters-mode 1)
  (hl-sexp-mode 1)
  (eldoc-mode 1)
  )

(setq inferior-lisp-program "sbcl")

;; - 'lisp-indent-function
;; - 'common-lisp-indent-function
;; (setq lisp-indent-function 'lisp-indent-function)


;;; [ rainbow-delimiters ] -- rainbow color parenthesis

(use-package rainbow-delimiters
  :ensure t
  :config
  ;; you have two styles:
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                      :foreground "#2aa198"
                      :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil
                      :foreground "#b58900"
                      :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil
                      :foreground "#268bd2"
                      :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil
                      :foreground "#dc322f"
                      :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil
                      :foreground "#859900"
                      :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil
                      :foreground "#268bd2"
                      :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil
                      :foreground "#cb4b16"
                      :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-8-face nil
                      :foreground "#d33682"
                      :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-9-face nil
                      :foreground "#839496"
                      :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground "orange" :background "black"
                      )
  (set-face-attribute 'rainbow-delimiters-mismatched-face nil
                      :foreground "red" :background "black"
                      )

  (rainbow-delimiters-mode t)
  ;; 1. global
  ;; (global-rainbow-delimiters-mode)
  ;; 2.. enable in all programming-related modes
  ;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  ;; 3. enable in specific modes
  (dolist (hook '(ruby-mode-hook
                  enh-ruby-mode-hook
                  ))
    (add-hook hook 'rainbow-delimiters-mode-enable))
  )


;;; [ paren-face ] -- A face dedicated to lisp parentheses.

(use-package paren-face
  ;; :ensure t
  ;; :config
  ;; (global-paren-face-mode 1)
  ;; (set-face-attribute 'parenthesis nil
  ;;                     :foreground "dim gray")
  )


;;; [ hl-sexp ] -- highlight the current sexp.

(use-package hl-sexp
  :ensure t
  :config
  (dolist (hook '(lisp-mode-hook
                  emacs-lisp-mode-hook
                  ))
    (add-hook hook #'hl-sexp-mode))

  (set-face-attribute 'hl-sexp-face nil
                      :background (color-darken-name (face-background 'default) 2)
                      )
  )


;;; [ eval-sexp-fu ] -- highlighting the sexps during evaluation in action.

(use-package eval-sexp-fu
  :ensure t
  :config
  (set-face-attribute 'eval-sexp-fu-flash nil
                      :background "#333333"
                      :weight 'normal
                      )
  (set-face-attribute 'eval-sexp-fu-flash-error nil
                      :foreground "red"
                      :weight 'bold)

  (setq eval-sexp-fu-flash-duration 0.5
        eval-sexp-fu-flash-error-duration 1.5
        ;; eval-sexp-fu-flash-function
        ;; eval-sexp-fu-flash-doit-function
        )

  (eval-sexp-fu-flash-mode 1)
  )


;;; [ parinfer-mode ] -- Parinfer on Emacs with oakmac's parinfer-elisp.

;; (use-package parinfer
;;   :ensure t
;;   :bind
;;   (;; Use this to toggle Indent/Paren Mode.
;;    ("C-," . parinfer-toggle-mode)
;;    ;; Some other commands you may want.
;;    ("M-r" . parinfer-raise-sexp)
;;    ("M-m" . mark-sexp)
;;    ("M-j" . parinfer-transpose-sexps)
;;    ("M-k" . parinfer-reverse-transpose-sexps))
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;;   (add-hook 'clojure-mode-hook #'parinfer-mode)
;;   )


(provide 'init-my-prog-lang-lisp)

;;; init-my-prog-lang-lisp.el ends here
