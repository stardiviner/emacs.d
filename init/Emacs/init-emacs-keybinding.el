;;; init-emacs-keybinding.el --- init Emacs' keybinding.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; A key binding is a mapping (relation) between an Emacs command and a key
;; sequence. The same command can be bound to more than one key sequence. A
;; given key sequence is the binding of at most one command in any given context
;; (e.g. any given buffer). The same key sequence can be bound to different
;; commands in different contexts and different keymaps.

;; A keymap is a collection of key bindings, so it is a mapping (relation)
;; between Emacs commands and key sequences. A keymap can be global, local, or
;; applicable only to a minor mode.


;;; Code:

;;; [ Modifiers]

;; (setq x-hyper-keysym 'hyper)

;;; stop using the arrow keys
(global-unset-key [left])
(global-unset-key [right])
(global-unset-key [up])
(global-unset-key [down])
(global-unset-key [next]) ; PageDown
(global-unset-key [prior]) ; PageUp


;;; bind some useful commands to keybindings.

(global-set-key (kbd "M-]") 'forward-sentence)
(global-set-key (kbd "M-[") 'backward-sentence)

;;; echo keystrokes in echo-area in seconds.
(setq-default echo-keystrokes 0.1)

;;; [ which-key ] -- Display available keybindings in popup.

(use-package which-key
  :ensure t
  :defer t
  :delight which-key-mode
  :commands (which-key-mode)
  :bind ("C-h C-h" . which-key-show-top-level)
  :init (which-key-mode 1)
  (setq which-key-side-window-location '(bottom right)))


;;; [ hydra ] -- tie related commands into a family of short bindings with a common prefix - a Hydra.

(use-package hydra
  :ensure t
  :defer t
  ;; :init
  ;; (use-package hydra-posframe ; a hydra extension which shows hydra hints on posframe.
  ;;   :ensure t
  ;;   :defer t
  ;;   :hook (after-init . hydra-posframe-enable))
  )

;;; [ hydra-posframe ] -- Display Hydra diagnostics at point.

(use-package hydra-posframe
  :ensure t
  :init (hydra-posframe-enable))

;;; [ pretty-hydra ] -- A macro for creating nice-looking Hydras.

(use-package pretty-hydra
  :ensure t)

;;; [ major-mode-hydra ] -- Major mode keybindings managed by Hydra.

(use-package major-mode-hydra
  :ensure t
  :bind ("S-SPC" . major-mode-hydra) ; [Shift-SPACE]
  :config
  (major-mode-hydra-define emacs-lisp-mode nil
    ("Eval"
     (("b" eval-buffer "buffer")
      ("e" eval-defun "defun")
      ("r" eval-region "region"))
     "REPL"
     (("I" ielm "ielm"))
     "Test"
     (("t" ert "prompt")
      ("T" (ert t) "all")
      ("F" (ert :failed) "failed"))
     "Doc"
     (("d" describe-foo-at-point "thing-at-pt")
      ("f" describe-function "function")
      ("v" describe-variable "variable")
      ("i" info-lookup-symbol "info lookup"))))

  (major-mode-hydra-define clojure-mode
    (:title "Clojure Mode" :color pink :separator "-")
    ("Load"
     (("k" cider-load-buffer "buffer" :exit nil)
      ("l" cider-load-file "file" :color red))))

  (major-mode-hydra-define go-mode
    (:title "Go Commands")
    ("Doc"
     (("d" godoc-at-point "doc at point"))
     "Imports"
     (("ia" go-import-add "add")
      ("ir" go-remove-unused-imports "cleanup")))))

;;; [ emaps ] -- Emaps provides utilities for working with keymaps and keybindings in Emacs.

(use-package emaps
  :ensure t
  :defer t
  :bind (("C-h C-k" . Info-goto-emacs-key-command-node) ; revert original function.
         ("C-h K" . emaps-describe-keymap-bindings)))

;;; Search Keybindings

(defun search-keybind (regexp &optional nlines)
  "Occur search the full list of keybinds & their commands. Very
helpful for learning and remembering forgotten binds."
  (interactive (occur-read-primary-args))
  (save-excursion
    (describe-bindings)
    (set-buffer "*Help*")
    (occur regexp nlines)
    (delete-windows-on "*Help*")))

(define-key help-map (kbd "M-s") 'search-keybind)

;;; [ key-quiz ] -- Key Quiz game for GNU Emacs.

(use-package key-quiz
  :ensure t
  :defer t
  :commands (key-quiz))


(provide 'init-emacs-keybinding)

;;; init-emacs-keybinding.el ends here
