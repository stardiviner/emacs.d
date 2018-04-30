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
  :mode ("\\.\\(?:a\\|so\\)\\'" . elf-mode))

;;; [ intel-hex-mode ] -- An Emacs mode for Intel hex files.

(use-package intel-hex-mode
  :ensure t
  :defer t)


(provide 'init-tool-hex)

;;; init-tool-hex.el ends here
