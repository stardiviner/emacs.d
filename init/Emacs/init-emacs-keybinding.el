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


;;; [ which-key ] -- Display available keybindings in popup.

(use-package which-key
  :ensure t
  :defer t
  :delight which-key-mode
  :commands (which-key-mode)
  :bind ("C-h C-h" . which-key-show-top-level)
  :init (which-key-mode 1))


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
