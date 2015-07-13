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



;;; [ alchemist ] -- Elixir Tooling Integration into Emacs

(require 'alchemist)

;;; Mix setup
;; use a different shell command for mix.
;; (setq alchemist-mix-command "/usr/local/bin/mix")
;; use a different task for running tests.
;; (setq alchemist-mix-test-task "espec")
;; use custom mix test task options.
;; (setq alchemist-mix-test-default-options "--exclude pending:true") ; default
;; use a different environment variable in which mix tasks will run.
;; (setq alchemist-mix-env "prod")

;;; IEx setup
;; use a different shell command for iex.
;; (setq alchemist-iex-program-name "/usr/local/bin/iex") ; default: iex

;;; Execute setup
;; use a different shell command for elixir.
;; (setq alchemist-compile-command "/usr/local/bin/elixir")

;;; Compile setup
;; use a different shell command for elixirc.
;; (setq alchemist-compile-command "/usr/local/bin/elixirc") ; default: elixirc

;;; Modeline setup
;; disable the change of the modeline color with the last compilation status.
;; (setq alchemist-buffer-status-modeline nil)

;;; Keybindings
;; use a different keybinding prefix than [C-c a]
;; (setq alchemist-key-command-prefix (kbd "C-c ,")) ; default: (kbd "C-c a")

;;; Testing Mode
;; disable the use of a more significant syntax highlighting on functions like test, assert_* and refute_*
;; (setq alchemist-test-mode-highlight-tests nil) ; default t

;;; Hooks
;; run the whole test suite with alchemist-mix-test after saving a buffer.
;; (setq alchemist-hooks-test-on-save t)


(provide 'init-my-prog-lang-elixir)

;;; init-my-prog-lang-elixir.el ends here
