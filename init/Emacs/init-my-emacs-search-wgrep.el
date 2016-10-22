;;; init-my-emacs-search-wgrep.el --- init for wgrep.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ wgrep ] -- writable grep buffer and apply the changes to files.

(use-package wgrep
  :ensure t
  :defer t
  :init
  (setq wgrep-enable-key (kbd "C-c C-p")
        wgrep-auto-save-buffer nil)
  )


;;; [ wgrep-ag ]

(use-package wgrep-ag
  :ensure t
  :defer t
  :init
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  )


(provide 'init-my-emacs-search-wgrep)

;;; init-my-emacs-search-wgrep.el ends here
