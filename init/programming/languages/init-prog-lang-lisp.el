;;; init-prog-lang-lisp.el --- Lisp dialects init
;;
;;; Commentary:

;;; Code:

;;; [ Lisp ]

;;; Common Settings for All Lisp dialects

;; for command `run-lisp'.
(setq inferior-lisp-program "sbcl")

;;; [ lisp-extra-font-lock ] -- Highlight bound variables and quoted expressions in Lisp.

;; (use-package lisp-extra-font-lock
;;   :ensure t
;;   :config (lisp-extra-font-lock-global-mode 1))

;;; [ Par Edit (paredit) ] -- performing structured editing of S-expression data.

(use-package paredit
  :ensure t
  :defer t
  :delight paredit-mode
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (lisp-interactive-mode . paredit-mode)
         (common-lisp-mode . paredit-mode)
         (scheme-mode . paredit-mode)
         (clojure-mode . paredit-mode)
         (clojurescript-mode . paredit-mode))
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
                                        eldoc-eval-expression)
    
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
                                        (line-beginning-position) t)))))))

;;; [ rainbow-delimiters ] -- rainbow color parenthesis

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :delight rainbow-delimiters-mode
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (lisp-mode . rainbow-delimiters-mode)
         (lisp-interaction-mode . rainbow-delimiters-mode)
         (common-lisp-mode . rainbow-delimiters-mode)
         (scheme-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (clojurescript-mode . rainbow-delimiters-mode)))

;;; [ rainbow-blocks-bg ] -- rainbow background highlighting of code blocks.

;; (use-package rainbow-blocks-bg
;;   :quelpa (rainbow-blocks-bg :fetcher github :repo "seanirby/rainbow-blocks-bg")
;;   :hook (clojure-mode . rainbow-blocks-bg-mode))

;;; [ highlight-blocks-bg ]

(use-package highlight-blocks
  :ensure t
  ;; :config
  ;; (defun highlight-blocks--rainbow-colors (theme)
  ;;   "Set highlight-blocks background colors list based on color theme light or dark."
  ;;   (cl-case (alist-get 'background-mode (frame-parameters))
  ;;     ('light
  ;;      (setq highlight-blocks--rainbow-colors
  ;;            '("#BAFFFF" "#FFCACA" "#FFFFBA" "#CACAFF" "#CAFFCA" "#FFBAFF")))
  ;;     ('dark
  ;;      (setq highlight-blocks--rainbow-colors
  ;;            '("#002200" "#00"
  ;;              ;; "grey55" "#93a8c6" "#b0b1a3" "#97b098" "#aebed8"
  ;;              ;; "#b0b0b3" "#90a890" "#a2b6da" "#9cb6ad"
  ;;              )))))
  ;;
  ;; (add-hook 'circadian-after-load-theme-hook #'highlight-blocks--rainbow-colors)
  ;;
  ;; (setq highlight-blocks-max-face-count
  ;;       (length highlight-blocks--rainbow-colors))
  ;;
  ;; (defun highlight-blocks--define-rainbow-colors (colors)
  ;;   (dotimes (i (length colors))
  ;;     (face-spec-set
  ;;      (intern (format "highlight-blocks-depth-%d-face" (1+ i)))
  ;;      `((((class color) (background dark))  :background ,(nth i colors))
  ;;        (((class color) (background light)) :background ,(nth i colors)))
  ;;      'face-defface-spec)))
  ;;
  ;; (highlight-blocks--define-rainbow-colors highlight-blocks--rainbow-colors)
  ;;
  ;; (highlight-blocks-mode 1)
  )

;;; [ hl-sexp ] -- highlight the current sexp.

(use-package hl-sexp
  ;; :quelpa (hl-sexp :fetcher github :repo "stardiviner/hl-sexp")
  :load-path "~/Code/Emacs/hl-sexp"     ; use my modified fork
  :defer t
  :commands (global-hl-sexp-mode hl-sexp-mode)
  :hook ((emacs-lisp-mode . hl-sexp-mode)
         (lisp-mode . hl-sexp-mode)
         (common-lisp-mode . hl-sexp-mode)
         (scheme-mode . hl-sexp-mode)
         (clojure-mode . hl-sexp-mode)
         (clojurescript-mode . hl-sexp-mode))
  :custom-face (hl-sexp-face ((t (:background ,(color-darken-name (face-background 'default) 5)
                                              :extend t))))
  :init
  (defun my/hl-sexp-set-face-background-color (theme)
    (set-face-attribute 'hl-sexp-face nil
                        :background (color-darken-name (face-background 'default) 5)
                        :extend t))
  (add-hook 'load-theme-after-hook #'my/hl-sexp-set-face-background-color))

;;; [ eval-sexp-fu ] -- highlighting the sexps during evaluation in action.

;; (use-package eval-sexp-fu
;;   :ensure t
;;   :defer t
;;   :custom ((eval-sexp-fu-flash-duration 0.5)
;;            (eval-sexp-fu-flash-error-duration 1.5)
;;            ;; (eval-sexp-fu-flash-function)
;;            ;; (eval-sexp-fu-flash-doit-function)
;;            )
;;   :init (eval-sexp-fu-flash-mode))

;;; [ parinfer-mode ] -- Parinfer on Emacs with oakmac's parinfer-elisp.

;; (use-package parinfer
;;   :ensure t
;;   :defer t
;;   :bind (;; Use this to toggle Indent/Paren Mode.
;;          ("C-," . parinfer-toggle-mode)
;;          ;; Some other commands you may want.
;;          ("M-r" . parinfer-raise-sexp)
;;          ("M-m" . mark-sexp)
;;          ("M-j" . parinfer-transpose-sexps)
;;          ("M-k" . parinfer-reverse-transpose-sexps))
;;   :hook ((emacs-lisp-mode . parinfer-mode)
;;          (clojure-mode . parinfer-mode)))


(provide 'init-prog-lang-lisp)

;;; init-prog-lang-lisp.el ends here
