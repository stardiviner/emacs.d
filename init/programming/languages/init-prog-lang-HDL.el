;;; init-prog-lang-HDL.el --- init for HDL
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ vhdl-mode ] -- major mode for editing VHDL code.

(use-package vhdl-mode
  :defer t
  :mode "\\.vhdl?\\'")

;;; [ vhdl-capf ] -- Completion at point function (capf) for vhdl-mode.

(use-package vhdl-capf
  :ensure t
  :commands (vhdl-capf-enable)
  :init (vhdl-capf-enable))

;;; [ vhdl-tools ] -- Utilities for navigating vhdl sources.

(use-package vhdl-tools
  :ensure t
  :hook (vhdl-mode . vhdl-tools-mode))


(provide 'init-prog-lang-HDL)

;;; init-prog-lang-HDL.el ends here
