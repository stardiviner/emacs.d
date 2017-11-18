;;; init-my-emacs-search-wgrep.el --- init for wgrep.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; writable grep or other search engine result buffer which can apply changes to files.

;;; Code:

;;; [ wgrep ] -- writable grep buffer and apply the changes to files.

(use-package wgrep
  :ensure t
  :init
  (setq wgrep-enable-key (kbd "C-c C-p")
        wgrep-auto-save-buffer nil)
  :config
  (advice-add 'wgrep-finish-edit :after #'wgrep-save-all-buffers)
  )


;;; [ wgrep-ag ]

(use-package wgrep-ag
  :ensure t
  :init
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (with-eval-after-load 'ag
    (add-hook 'ag-mode-hook 'wgrep-ag-setup))
  )


(provide 'init-my-emacs-search-wgrep)

;;; init-my-emacs-search-wgrep.el ends here
