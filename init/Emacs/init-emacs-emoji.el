;;; init-emacs-emoji.el --- init Emoji for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ display Unicode Emoji ]

;;; A font that supports emoji is needed. The best results are obtained with
;;; "Noto Color Emoji" or "Symbola". It might be necessary to instruct Emacs to
;;; use such font with a line like the following.
(set-fontset-font t 'symbol
                  (font-spec :family "Noto Color Emoji") nil 'prepend)

;;; [ emojify ] display emojis in Emacs.

(use-package emojify
  :ensure t
  :defer t
  :custom ((emojify-emojis-dir (concat user-emacs-directory "emojis"))
           (emojify-program-contexts '(comments string))
           ;; emojify-display-style 'unicode
           )
  :commands (global-emojify-mode)
  :init (global-emojify-mode 1))

;;; [ company-emoji ] -- company-mode backend providing completion for emoji.🆒💖

(use-package company-emoji
  :ensure t
  :defer t
  :commands (company-emoji)
  ;; :init (with-eval-after-load 'company
  ;;         (setq-default company-backends
  ;;                       (let ((default-value (default-value 'company-backends)))
  ;;                         (add-to-list 'company-backends 'company-emoji 'append)
  ;;                         )))
  )

;;; [ ivy-emoji ] -- Insert emojis with Ivy.

(use-package ivy-emoji
  :ensure t
  :defer t
  :commands (ivy-emoji))


;;; ----------------------------------------------------------------------------

(provide 'init-emacs-emoji)

;;; init-emacs-emoji.el ends here
