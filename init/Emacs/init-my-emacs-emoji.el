;;; init-my-emacs-emoji.el --- init Emoji for Emacs.

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
  ;; (add-to-list 'company-backends 'company-emoji)
  )


;;; ----------------------------------------------------------------------------

(provide 'init-my-emacs-emoji)

;;; init-my-emacs-emoji.el ends here
