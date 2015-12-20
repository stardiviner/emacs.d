;;; init-my-prog-lang-lisp.el --- Lisp dialects init
;;
;;; Commentary:

;;; Code:

;;; [ Lisp ]

;;; Common Settings for All Lisp dialects
(hook-modes lisp-dialects-mode
  (rainbow-delimiters-mode-enable)
  (enable-paredit-mode)
  (hl-sexp-mode 1)
  (eldoc-mode 1)
  (company-mode-on)
  )

(setq inferior-lisp-program "sbcl")

;; - 'lisp-indent-function
;; - 'common-lisp-indent-function
;; (setq lisp-indent-function 'lisp-indent-function)


;;; [ rainbow-delimiters ] -- rainbow color parenthesis

(require 'rainbow-delimiters nil 'noerror)
;; (when (require 'rainbow-delimiters nil 'noerror)
;;   )

(eval-after-load 'rainbow-delimiters
  '(progn
     (rainbow-delimiters-mode t)
     ;; 1. global
     ;; (global-rainbow-delimiters-mode)
     ;; 2. enable in all Lisp dialects modes
     ;; (hook-modes lisp-dialects-mode
     ;;   (rainbow-delimiters-mode-enable))
     ;; 3.. enable in all programming-related modes
     ;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
     ;; 4. enable in specific modes
     (dolist (hook '(ruby-mode-hook
                     enh-ruby-mode-hook
                     ))
       (add-hook hook 'rainbow-delimiters-mode-enable))
     )
  )

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


;; A quick way to jump to the definition of a function given its key binding
;; (global-set-key (kbd "C-h K") 'find-function-on-key)


;;; [ hl-sexp ]

(use-package hl-sexp
  :config
  (dolist (hook '(lisp-mode-hook
                  emacs-lisp-mode-hook
                  ))
    (add-hook hook #'hl-sexp-mode))

  (set-face-attribute 'hl-sexp-face nil
                      :background (color-darken-name (face-background 'default) 2)
                      )
  )


;;; [ eval-sexp-fu ] -- You can see highlighting the sexps during evaluation in action.

;;; Usage:
;;
;;  `eval-sexp-fu-flash-mode'
;;    Toggle EvalSexpFuFlash mode on or off.
;;    If this mode is on, some `eval-last-sexp'-ish commands will highlight the sexps during evaluation.
;;  `turn-on-eval-sexp-fu-flash-mode'
;;    Unequivocally turn on EvalSexpFuFlash mode
;;  `eval-sexp-fu-eval-sexp-inner-list'
;;    Evaluate the list _currently_ pointed at as sexp; print value in minibuffer.
;;  `eval-sexp-fu-eval-sexp-inner-sexp'
;;    Evaluate the sexp _currently_ pointed; print value in minibuffer.

(require 'eval-sexp-fu)

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


(provide 'init-my-prog-lang-lisp)

;;; init-my-prog-lang-lisp.el ends here
