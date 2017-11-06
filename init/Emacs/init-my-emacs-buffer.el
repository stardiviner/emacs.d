;;; init-my-emacs-buffer.el --- init Emacs buffer settings.

;;; Commentary:



;;; Code:

;;; [ auto-revert-mode ] -- auto-reload external file changes.

(setq global-auto-revert-non-file-buffers nil
      auto-revert-verbose nil
      auto-revert-use-notify t)

;; (global-auto-revert-mode)

;;; [ ibuffer ]

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq buffers-menu-max-size 30)
  )

;;; [uniquify] -- meaningful names for buffers with the same name

(require 'uniquify)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*" ; don't muck with special buffers
      )

;;; [ recentf ] -- setup a menu of recently opened files.

(require 'recentf)

(recentf-mode)

(setq recentf-save-file (expand-file-name ".temp/recentf" user-emacs-directory)
      recentf-exclude '("/tmp/" "/ssh:")
      ;; recentf-auto-cleanup 'mode ; 'mode, 'never.
      )


(provide 'init-my-emacs-buffer)

;;; init-my-emacs-buffer.el ends here
