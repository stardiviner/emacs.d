;;; init-my-prog-lang-elixir.el --- init for Elixir
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ elixir-mode ] -- Emacs major mode for Elixir

;; if you use smartparens you can piggyback on some of its functionality for
;; dealing with Ruby's do .. end blocks. A sample configuration would be:
(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
                 :when '(("SPC" "RET"))
                 :actions '(insert navigate))
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(sp-ruby-def-post-handler)
                 :actions '(insert navigate)))



;;; [ alchemist ] -- Elixir Tooling Integration into Emacs

;;; Usage:
;;
;; - prefix [C-c a]

(require 'alchemist)

(add-hook 'elixir-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'alchemist-company)))

(use-package alchemist
  :config
  (setq alchemist-key-command-prefix (kbd "C-c ,")) ; default: (kbd "C-c a")
  ;; run the whole test suite with `alchemist-mix-test' after saving a buffer.
  (setq alchemist-hooks-test-on-save nil)
  )


(provide 'init-my-prog-lang-elixir)

;;; init-my-prog-lang-elixir.el ends here
