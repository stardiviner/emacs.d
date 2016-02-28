;;; init-my-prog-lang-elixir.el --- init for Elixir
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ elixir-mode ] -- Emacs major mode for Elixir

(use-package elixir-mode
  :ensure t
  :config
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
  )


;;; [ alchemist ] -- Elixir Tooling Integration into Emacs

;;; Usage:
;;
;; - prefix [C-c a]

(use-package alchemist
  :ensure t
  :config
  (setq alchemist-key-command-prefix (kbd "C-c ,")) ; default: (kbd "C-c a")
  ;; run the whole test suite with `alchemist-mix-test' after saving a buffer.
  (setq alchemist-hooks-test-on-save nil)

  (add-hook 'elixir-mode-hook
            (lambda ()
              (my-company-add-backends-to-mode '(alchemist-company))))
  )


;;; [ ob-elixir ]

(use-package ob-elixir)


(provide 'init-my-prog-lang-elixir)

;;; init-my-prog-lang-elixir.el ends here
