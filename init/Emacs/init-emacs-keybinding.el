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

;;; shorter suggestion:
;; You can run the command ‘find-file’ with C-x C-f
;; =>
;; You can run the command ‘emacs-uptime’ with M-x -upt RET
(setq extended-command-suggest-shorter nil)

;; (defun display-extended-command-shorter (command)
;;   "Display information on a shorter way to M-x a command."
;;   (interactive (list (read-extended-command)))
;;   (message "The command `%s' can be invoked with `M-x %s'"
;;            command
;;            (execute-extended-command--shorter command command)))


(setq suggest-key-bindings t) ; show the /equivalent/ key binding when [M-x] command has one.

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


;;; [ which-key ]

(use-package which-key
  :ensure t
  :defer t
  :init (which-key-mode 1)
  :bind ("C-h C-h" . which-key-show-top-level)
  :config
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay nil)

  (setq which-key-max-description-length 27
        ;; which-key-separator " ⇢ "
	      which-key-separator " "
        ;; which-key-unicode-correction 3
        which-key-show-prefix 'mode-line ; 'mode-line 'echo 'left
        which-key-show-remaining-keys t
        )

  (setq which-key-sort-order 'which-key-key-order-alpha)
  
  (setq which-key-highlighted-command-list
        '("toggle"
          "register" "bookmark"
          "rectangle" "iedit"
	        "describe"
          ("emacs" . highlight)
          ))

  (set-face-attribute 'which-key-highlighted-command-face nil
		                  :underline nil :weight 'bold)
  )


;;; [ hydra ] -- tie related commands into a family of short bindings with a common prefix - a Hydra.

(use-package hydra
  :ensure t
  :defer t
  :config
  ;; display a hint with possible bindings in the echo area.
  (setq hydra-is-helpful t)

  ;; ace-window + hydra
  (global-set-key
   (kbd "C-x C-z")
   (defhydra hydra-window ()
     "window"
     ("h" windmove-left)
     ("j" windmove-down)
     ("k" windmove-up)
     ("l" windmove-right)
     ("s" (lambda ()
            (interactive)
            (split-window-right)
            (windmove-right))
      "split hor")
     ("v" (lambda ()
            (interactive)
            (split-window-below)
            (windmove-down))
      "split vert")
     ("t" transpose-frame "trans frame" :color blue)
     ("f" my-turn-current-window-into-new-frame "frame" :color blue)
     ("o" delete-other-windows "one" :color red)
     ("a" ace-window "ace" :color cyan)
     ("C-j" ace-window "ace" :color cyan)
     ("s" ace-swap-window "swap" :color yellow)
     ("d" delete-window "del" :color red)
     ("m" ace-maximize-window "max" :color orange)
     ("b" bookmark-jump "bookmark" :color blue)
     ("C-x" bm-toggle "bm (toggle)" :color yellow)
     ("C-b" bm-previous "bm (prev)" :color yellow)
     ("C-f" bm-next "bm (next)" :color yellow)
     ("q" nil "cancel")
     )
   )
  )


;;; [ emaps ] -- Emaps provides utilities for working with keymaps and keybindings in Emacs.

(use-package emaps
  :ensure t
  :defer t
  :bind (("C-h C-k" . Info-goto-emacs-key-command-node) ; revert original function.
         ("C-h K" . emaps-describe-keymap-bindings))
  )

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


(provide 'init-emacs-keybinding)

;;; init-emacs-keybinding.el ends here
