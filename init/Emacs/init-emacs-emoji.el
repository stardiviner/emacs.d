;;; init-emacs-emoji.el --- init Emoji for Emacs.

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------

;;; [ emojify ] display emojis in Emacs.

(use-package emojify
  :ensure t
  :config
  (setq emojify-emojis-dir (concat user-emacs-directory "emojis")
        emojify-program-contexts '(comments string code)
        emojify-display-style 'image
        ;; emojify-point-entered-behaviour
        emojify-reveal-on-isearch t
        emojify-show-help t
        )
  ;; (add-to-list 'emojify-inhibit-major-modes ')

  (global-emojify-mode 1)
  )

;;; [ company-emoji ] -- company-mode backend providing completion for emoji. 🆒💦

(use-package company-emoji
  :ensure t
  :config
  (dolist (hook '(org-mode-hook
                  markdown-mode-hook
                  tex-mode-hook
                  latex-mode-hook))
    (add-hook hook
              (lambda ()
                (my-company-add-backend-locally 'company-emoji))))
  )


;;; ----------------------------------------------------------------------------

(provide 'init-emacs-emoji)

;;; init-emacs-emoji.el ends here
