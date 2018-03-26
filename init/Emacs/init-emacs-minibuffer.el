;;; init-emacs-minibuffer.el --- init Emacs minibuffer

;;; Commentary:

;;; Code:

;;; [ minibuffer ]

;; (setq-default max-mini-window-height 6)

;; recursive minibuffers
(setq enable-recursive-minibuffers t)   ; enable to use minibuffer recursively.
(if (booleanp enable-recursive-minibuffers)
    (minibuffer-depth-indicate-mode t))

;; minibuffer prompt face properties
(setq minibuffer-prompt-properties '(read-only t face minibuffer-prompt))

(minibuffer-electric-default-mode t)

(setq minibuffer-completion-confirm nil
      minibuffer-auto-raise t
      minibuffer-allow-text-properties t
      ;; minibuffer-frame-alist
      ;; minibuffer-history-position t
      )

(defun my/minibuffer-lisp-setup ()
  "Setup minibuffer for Lisp editing."
  (eldoc-mode 1)
  (with-eval-after-load 'rainbow-delimiters
    (rainbow-delimiters-mode-enable))
  (with-eval-after-load 'smartparens
    (smartparens-strict-mode 1)))
(add-hook 'eval-expression-minibuffer-setup-hook #'my/minibuffer-lisp-setup)

;;; - `eval-expression-minibuffer-setup-hook'
;;
;; (setq eval-expression-debug-on-error t
;;       eval-expression-print-level nil ; 4, nil,
;;       eval-expression-print-length nil
;;       )

;;; [ echo area ]


;;; [ help-at-pointer ]

;; (setq help-at-pt-display-when-idle t
;;       ;; help-at-pt-timer-delay 0.1
;;       )
;; (help-at-pt-set-timer)


(provide 'init-emacs-minibuffer)

;;; init-emacs-minibuffer.el ends here
