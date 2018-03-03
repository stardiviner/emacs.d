;;; init-tool-hex.el --- init for Hex.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Hexadecimal ]




;;; [ hexl-mode ]

;; (setq hexl-follow-ascii t
;;       )


;;; [ hexview-mode ]

;; (use-package hexview-mode
;;   :ensure t
;;   :config
;;   (define-key prog-code-prefix (kbd "h") 'hexview-find-file)
;;   )


;;; [ elf-mode ] -- Show symbols in binaries.

(use-package elf-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.\\(?:a\\|so\\)\\'" . elf-mode))
  )


(provide 'init-tool-hex)

;;; init-tool-hex.el ends here
