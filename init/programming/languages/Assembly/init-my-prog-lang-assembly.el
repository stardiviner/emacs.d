;;; init-my-prog-lang-assembly.el --- init for Assembly languages
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ asm-mode ]


;;; [ nasm-mode ]


;;; [ fasm-mode ]


;;; [ iasm-mode ]

;; (global-set-key (kbd "C-c C-d") 'iasm-disasm)
;; (global-set-key (kbd "C-c C-l") 'iasm-ldd)

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c d") 'iasm-goto-disasm-buffer)
            (local-set-key (kbd "C-c l") 'iasm-disasm-link-buffer)))


;;; [ gas-mode ]


;;; [ haxor-mode ] -- Major mode for editing Haxor Assembly Files.

(use-package haxor-mode
  :defer t
  :mode ("\\.hax\\'" . haxor-mode)
  )


(provide 'init-my-prog-lang-assembly)

;;; init-my-prog-lang-assembly.el ends here
