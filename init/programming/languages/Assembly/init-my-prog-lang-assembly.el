;;; init-my-prog-lang-assembly.el --- init for Assembly languages
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ asm-mode ]

(require 'asm-mode)

(defun my-asm-mode-settings ()
  ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
  (local-unset-key (vector asm-comment-char))
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
  (setq-local tab-always-indent (default-value 'tab-always-indent))
  )

(add-hook 'asm-mode-hook #'my-asm-mode-settings)


;;; [ nasm-mode ] -- NASM x86 assembly major mode.

(use-package nasm-mode
  :ensure t)


;;; [ fasm-mode ]


;;; [ iasm-mode ] -- interactive assembly major mode.

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


;;; [ llvm-mode ] -- Major mode for the LLVM assembler language.

(use-package llvm-mode
  :ensure t)


;;; [ x86-lookup ] -- jump to x86 instruction documentation.

(use-package x86-lookup
  :ensure t
  :config
  (setq x86-lookup-pdf (concat
                        user-emacs-directory
                        "documentations/Assembly/NASM/"
                        "Combined Volume Set of Intel 64 and IA-32 Architectures Software Developer's Manuals.pdf"))
  (setq x86-lookup-browse-pdf-function 'x86-lookup-browse-pdf-pdf-tools)
  (define-key asm-mode-map (kbd "C-h d d") #'x86-lookup)
  )


(provide 'init-my-prog-lang-assembly)

;;; init-my-prog-lang-assembly.el ends here
