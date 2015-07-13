;;; init-my-prog-lang-elixir.el --- init for Elixir
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ emacs-elixir ] -- Emacs major mode for Elixir

;; this mode is based on package elixir-mode.

(require 'elixir-mode)

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




(provide 'init-my-prog-lang-elixir)

;;; init-my-prog-lang-elixir.el ends here
