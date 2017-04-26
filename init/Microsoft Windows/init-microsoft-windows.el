;;; init-microsoft-windows.el --- init for Microsoft Windows.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ ini-mode ] -- major mode for Windows-style ini files.

(use-package ini-mode
  :ensure t
  :mode "\\.ini\\'")

;;; [ dos ] -- major mode for editing DOS scripts.

;; (use-package dos
;;   :ensure t
;;   :defer t)

;;; [ bat-mode ] -- major mode for editing DOS/Windows scripts.

;; (use-package bat-mode
;;   :ensure t
;;   :defer t)

;;; [ batch-mode ] --

;; (use-package batch-mode
;;   :ensure t
;;   :defer t)

;;; ----------------------------------------------------------------------------

(provide 'init-microsoft-windows)

;;; init-microsoft-windows.el ends here
