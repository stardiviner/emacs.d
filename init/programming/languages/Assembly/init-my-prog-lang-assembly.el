;;; init-my-prog-lang-assembly.el --- init for Assembly languages
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ asm-mode ]

(use-package asm-mode
  :ensure t
  :config
  )


;;; [ nasm-mode ]

(use-package nasm-mode
  :ensure t)


;;; [ fasm-mode ]


;;; [ iasm-mode ]

(use-package iasm-mode
  :ensure t
  :config
  (define-key iasm-mode-map (kbd "C-c C-d") 'iasm-disasm)
  (define-key iasm-mode-map (kbd "C-c d") 'iasm-goto-disasm-buffer)
  (define-key iasm-mode-map (kbd "C-c C-l") 'iasm-ldd)
  (define-key iasm-mode-map (kbd "C-c l") 'iasm-disasm-link-buffer)
  )


;;; [ gas-mode ]


;;; [ haxor-mode ] -- Major mode for editing Haxor Assembly Files.

(use-package haxor-mode
  ;; :ensure t
  :defer t
  :mode ("\\.hax\\'" . haxor-mode)
  )


(provide 'init-my-prog-lang-assembly)

;;; init-my-prog-lang-assembly.el ends here
