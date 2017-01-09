;;; init-my-prog-lang-lisp.el --- Lisp dialects init
;;
;;; Commentary:

;;; Code:

;;; [ Lisp ]

;;; Common Settings for All Lisp dialects

(setq inferior-lisp-program "sbcl")


;;; [ Par Edit (paredit) ] -- performing structured editing of S-expression data.

(use-package paredit
  :ensure t
  :defer t
  :config
  ;; remove following keybindings
  (dolist (binding (list (kbd "C-<left>") (kbd "C-<right>")
                         (kbd "C-M-<left>") (kbd "C-M-<right>")))
    (define-key paredit-mode-map binding nil))
  ;; disable kill-sentence, which is easily confused with the kill-sexp binding,
  ;; but doesn't preserve sexp structure
  (define-key paredit-mode-map [remap kill-sentence] nil)
  (define-key paredit-mode-map [remap backward-kill-sentence] nil)

  ;; use paredit in the minibuffer
  (defvar paredit-minibuffer-commands '(eval-expression
                                        pp-eval-expression
                                        eval-expression-with-eldoc
                                        ibuffer-do-eval
                                        ibuffer-do-view-and-eval
                                        eldoc-eval-expression
                                        )
    "Interactive commands for which paredit should be enabled in the minibuffer.")
  (defun conditionally-enable-paredit-mode ()
    "Enable paredit during lisp-related minibuffer commands."
    (if (memq this-command paredit-minibuffer-commands)
        (enable-paredit-mode)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

  ;; patches

  ;; fix paredit auto add space before ( in circular list refs. #1= (a b c . #1#)
  (add-to-list 'paredit-space-for-delimiter-predicates 'no-space-for-list-refs)

  (defun no-space-for-list-refs (endp delimiter)
    (or endp
        (save-excursion
          (not (and (eq delimiter ?\()
                    (re-search-backward (rx "#" (+ (any "0-9")) "=")
                                        (line-beginning-position) t))))))

  ;; fix paredit auto add space before ( in list splicing. ",@()".
  (add-to-list 'paredit-space-for-delimiter-predicates 'no-space-for-list-splicing)

  (defun no-space-for-list-splicing (endp delimiter)
    (or endp
        (save-excursion
          (not (and (eq delimiter ?\()
                    (re-search-backward (rx ",@")
                                        (line-beginning-position) t))))))
  )

;;; [ paxedit ] -- Structured, Context-Driven, LISP Editing and Refactoring.

(use-package paxedit
  :ensure t
  :defer t)

;;; [ rainbow-delimiters ] -- rainbow color parenthesis

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  ;; (rainbow-delimiters-mode t)
  ;; 1. global
  ;; (global-rainbow-delimiters-mode)
  ;; 2.. enable in all programming-related modes
  ;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  ;; 3. enable in specific modes
  ;; (dolist (hook '(ruby-mode-hook
  ;;                 enh-ruby-mode-hook
  ;;                 ))
  ;;   (add-hook hook 'rainbow-delimiters-mode-enable))

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
  )


;;; [ hl-sexp ] -- highlight the current sexp.

(use-package hl-sexp
  :ensure t
  :config
  (set-face-attribute 'hl-sexp-face nil
                      :background (color-darken-name (face-background 'default) 3)
                      )
  )


;;; [ eval-sexp-fu ] -- highlighting the sexps during evaluation in action.

(use-package eval-sexp-fu
  :ensure t
  :config
  (setq eval-sexp-fu-flash-duration 0.5
        eval-sexp-fu-flash-error-duration 1.5
        ;; eval-sexp-fu-flash-function
        ;; eval-sexp-fu-flash-doit-function
        )

  (set-face-attribute 'eval-sexp-fu-flash nil
                      :background "#333333"
                      :weight 'normal
                      )
  (set-face-attribute 'eval-sexp-fu-flash-error nil
                      :foreground "red"
                      :weight 'bold)

  (eval-sexp-fu-flash-mode 1)
  )


;;; [ parinfer-mode ] -- Parinfer on Emacs with oakmac's parinfer-elisp.

;; (use-package parinfer
;;   :ensure t
;;   :defer t
;;   :bind
;;   (;; Use this to toggle Indent/Paren Mode.
;;    ("C-," . parinfer-toggle-mode)
;;    ;; Some other commands you may want.
;;    ("M-r" . parinfer-raise-sexp)
;;    ("M-m" . mark-sexp)
;;    ("M-j" . parinfer-transpose-sexps)
;;    ("M-k" . parinfer-reverse-transpose-sexps))
;;   :init
;;   (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;;   (add-hook 'clojure-mode-hook #'parinfer-mode)
;;   )



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

;; I already set upper Lisp common settings function in separately init files.
;; (dolist (hook '(lisp-mode-hook
;;                 emacs-lisp-mode-hook
;;                 clojure-mode-hook
;;                 ))
;;   (add-hook hook
;;             (lambda ()
;;               (my-lisp-common-settings))))


(provide 'init-my-prog-lang-lisp)

;;; init-my-prog-lang-lisp.el ends here
