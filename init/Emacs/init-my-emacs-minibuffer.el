;;; init-my-emacs-minibuffer.el --- init Emacs minibuffer

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
(set-face-attribute 'minibuffer-prompt nil
                    :inverse-video nil
                    :weight 'normal :slant 'normal :underline nil
                    :foreground "white" :background "DarkSlateGray"
                    )

(minibuffer-electric-default-mode t)

(setq minibuffer-completion-confirm nil
      minibuffer-auto-raise t
      minibuffer-allow-text-properties t
      ;; minibuffer-frame-alist
      ;; minibuffer-history-position t
      )


(add-hook 'eval-expression-minibuffer-setup-hook
          #'(lambda ()
              (eldoc-mode 1)
              (with-eval-after-load 'rainbow-delimiters
                (rainbow-delimiters-mode-enable))
              (with-eval-after-load 'smartparens
                (smartparens-strict-mode 1))
              ;; (paredit-mode 1)
              ))


;;; [ echo area ]


;;; [ help-at-pointer ]

(setq help-at-pt-display-when-idle t
      ;; help-at-pt-timer-delay 0.1
      )
(help-at-pt-set-timer)


(provide 'init-my-emacs-minibuffer)

;;; init-my-emacs-minibuffer.el ends here
