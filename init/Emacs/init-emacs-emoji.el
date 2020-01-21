;;; init-emacs-emoji.el --- init Emoji for Emacs.

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------

;;; [ emojify ] display emojis in Emacs.

(use-package emojify
  :ensure t
  :init (setq emojify-emojis-dir (concat user-emacs-directory "emojis")
              ;; emojify-program-contexts '(comments string)
              emojify-display-style 'unicode)
  :config (global-emojify-mode 1)
  (emojify-mode-line-mode))

;;; [ company-emoji ] -- company-mode backend providing completion for emoji. 🆒💦

(use-package company-emoji
  :ensure t
  :init
  (dolist (hook '(org-mode-hook
                  markdown-mode-hook
                  tex-mode-hook
                  latex-mode-hook))
    (add-hook hook
              (lambda () (my-company-add-backend-locally 'company-emoji)))))


;;; ----------------------------------------------------------------------------

(provide 'init-emacs-emoji)

;;; init-emacs-emoji.el ends here
