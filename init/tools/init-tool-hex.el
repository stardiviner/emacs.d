;;; init-tool-hex.el --- init for Hex.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ hexl ] -- hexlify and dehexlify binary file content.

(use-package hexl
  :defer t
  :commands (hexl-mode hexl-find-file hexlify-buffer))

;;; [ nhexl-mode ] -- Minor mode to edit files via hex-dump format.

(use-package nhexl-mode
  :ensure t
  :defer t
  :commands (nhexl-mode)
  :hook (hexl-mode . nhexl-mode)
  :config
  ;; hexl-isearch: Let isearch search the binary instead of the hexl buffer.
  (with-eval-after-load 'hexl-mode
    (load (expand-file-name "init/extensions/hexl-isearch.el" user-emacs-directory))))

;;; [ hexview-mode ]

;; (use-package hexview-mode
;;   :ensure t
;;   :defer t
;;   :config (define-key prog-code-prefix (kbd "h") 'hexview-find-file))

;;; [ elf-mode ] -- Show symbols in binaries.

(use-package elf-mode
  :ensure t
  :defer t
  :mode ("\\.\\(?:a\\|so\\)\\'" . elf-mode))

;;; [ intel-hex-mode ] -- An Emacs mode for Intel hex files.

(use-package intel-hex-mode
  :ensure t
  :defer t)

(provide 'init-tool-hex)

;;; init-tool-hex.el ends here
