;;; init-prog-lang-assembly.el --- init for Assembly languages
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ asm-mode ]

(use-package asm-mode
  :ensure t
  :defer t
  :config
  (defun my-asm-mode-settings ()
    ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
    (local-unset-key (vector asm-comment-char))
    ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
    (setq-local tab-always-indent (default-value 'tab-always-indent)))
  (add-hook 'asm-mode-hook #'my-asm-mode-settings))


;;; [ nasm-mode ] -- NASM x86 assembly major mode.

(use-package nasm-mode
  :ensure t
  :defer t)

;;; [ masm-mode ] -- Major mode for editing MASM assembly programs.

(use-package masm-mode
  :ensure t
  :defer t)

;;; [ fasm-mode ]


;;; [ iasm-mode ] -- interactive assembly major mode.

(use-package iasm-mode
  :ensure t
  :defer t
  :config
  (define-key iasm-mode-map (kbd "C-c C-d") 'iasm-disasm)
  (define-key iasm-mode-map (kbd "C-c d") 'iasm-goto-disasm-buffer)
  (define-key iasm-mode-map (kbd "C-c C-l") 'iasm-ldd)
  (define-key iasm-mode-map (kbd "C-c l") 'iasm-disasm-link-buffer))


;;; [ gas-mode ]


;;; [ haxor-mode ] -- Major mode for editing Haxor Assembly Files.

;; (use-package haxor-mode
;;   :ensure t
;;   :defer t
;;   :mode ("\\.hax\\'" . haxor-mode))


;;; [ mips-mode ] -- An Emacs major mode for MIPS assembly code.

(use-package mips-mode
  :ensure t
  :defer t
  :mode "\\.mips$")


;;; [ x86-lookup ] -- jump to x86 instruction documentation.

(use-package x86-lookup
  :ensure t
  :defer t
  :config
  (setq x86-lookup-pdf (concat
                        user-emacs-directory
                        "documentations/Assembly/NASM/"
                        "Intel 64 and IA-32 Architectures Software Developers Manuals: combined volumes 2A, 2B, 2C, and 2D: Instruction set reference, A-Z.pdf"))
  (setq x86-lookup-browse-pdf-function 'x86-lookup-browse-pdf-okular)
  (define-key asm-mode-map (kbd "C-h d d") #'x86-lookup))


(provide 'init-prog-lang-assembly)

;;; init-prog-lang-assembly.el ends here
