;;; init-emacs-buffer.el --- init Emacs buffer settings.

;;; Commentary:



;;; Code:

;;; [ auto-revert-mode ] -- auto-reload external file changes.

(use-package autorevert
  :defer t
  :delight auto-revert-mode
  :init (setq auto-revert-interval 30)
  :config (global-auto-revert-mode 1))

;;; [ ibuffer ]

(use-package ibuffer
  :ensure t
  :defer t
  :bind ("C-x C-b" . ibuffer)
  :init (setq buffers-menu-max-size nil))

(use-package all-the-icons-ibuffer
  :ensure t
  :init (all-the-icons-ibuffer-mode))

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
  :custom ((recentf-save-file (expand-file-name ".temp/recentf" user-emacs-directory))
           ;; (recentf-auto-cleanup 'mode) ; 'mode, 'never.
           (recentf-exclude '("/tmp/" "/ssh:")))
  :hook (after-init . recentf-mode))

;;; [ saveplace ] -- save visited files' point positions.

(use-package saveplace
  :hook (after-init . save-place-mode))

;;; [ frog-jump-buffer ] -- The fastest buffer-jumping Emacs lisp package around.

(use-package frog-jump-buffer
  :ensure t
  :defer t
  :commands (frog-jump-buffer)
  :bind ("C-;" . frog-jump-buffer))


(provide 'init-emacs-buffer)

;;; init-emacs-buffer.el ends here
