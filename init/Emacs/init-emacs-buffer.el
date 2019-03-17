;;; init-emacs-buffer.el --- init Emacs buffer settings.

;;; Commentary:



;;; Code:

;;; [ auto-revert-mode ] -- auto-reload external file changes.

(use-package autorevert
  :defer t
  :delight auto-revert-mode
  :init (setq global-auto-revert-non-file-buffers nil
              auto-revert-verbose nil
              auto-revert-use-notify t)
  :config (global-auto-revert-mode -1))

;;; [ ibuffer ]

(use-package ibuffer
  :ensure t
  :defer t
  :bind ("C-x C-b" . ibuffer)
  :init (setq buffers-menu-max-size nil))

;;; [uniquify] -- meaningful names for buffers with the same name

(use-package uniquify
  :defer t
  :init (setq uniquify-buffer-name-style 'post-forward-angle-brackets
              uniquify-separator " • "
              uniquify-after-kill-buffer-p t
              uniquify-ignore-buffers-re "^\\*" ; don't muck with special buffers
              ))

;;; [ recentf ] -- setup a menu of recently opened files.

(use-package recentf
  :defer t
  :init (setq recentf-save-file (expand-file-name ".temp/recentf" user-emacs-directory)
              recentf-exclude '("/tmp/" "/ssh:")
              ;; recentf-auto-cleanup 'mode ; 'mode, 'never.
              )
  :config (recentf-mode))


(provide 'init-emacs-buffer)

;;; init-emacs-buffer.el ends here
