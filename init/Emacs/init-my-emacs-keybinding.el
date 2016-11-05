;;; init-my-emacs-keybinding.el --- init Emacs' keybinding.
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


;;; Search Keybindings

;; Occur search the full list of keybinds & their commands. Very helpful for
;; learning and remembering forgotten binds.

(defun search-keybind (regexp &optional nlines)
  (interactive (occur-read-primary-args))
  (save-excursion
    (describe-bindings)
    (set-buffer "*Help*")
    (occur regexp nlines)
    (delete-windows-on "*Help*")))

(define-key help-map (kbd "C-k") 'search-keybind)


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
  :init
  ;; TODO: workaround for bug (void-function display-buffer-in-major-side-window)
  (defalias 'display-buffer-in-major-side-window 'window--make-major-side-window)
  :bind (("C-x C-t" . which-key-show-top-level))
  :config
  (setq which-key-idle-delay 1.5
        which-key-idle-secondary-delay nil)

  (setq which-key-max-description-length 27
        which-key-separator " ➜ " ; ⇢
        ;; which-key-unicode-correction 3
        which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL")
        ;; which-key-prefix-title-alist '()
        which-key-show-prefix 'echo ; 'left 'echo
        which-key-show-remaining-keys t
        )

  (setq which-key-sort-order 'which-key-key-order-alpha)
  
  (setq which-key-highlighted-command-list
        '("toggle"
          "register" "bookmark"
          "rectangle" "iedit"
          ("emacs" . highlight)
          ))

  (which-key-mode 1)
  )


;;; [ hydra ] -- tie related commands into a family of short bindings with a common prefix - a Hydra.

(use-package hydra
  :ensure t
  :config
  ;; display a hint with possible bindings in the echo area.
  (setq hydra-is-helpful t)

  ;; examples
  (require 'hydra-examples)
  
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
     ("z" zoom-window-zoom "zoom" :color orange)
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
  :config
  (global-set-key (kbd "C-h C-k") 'Info-goto-emacs-key-command-node) ; revert original function.
  (global-set-key (kbd "C-h K") 'emaps-describe-keymap-bindings)
  )


;;; [ Buffer-locally overriding minor-mode key bindings in Emacs ]

;;; Solution 1.
;;; ---------------------------------------------------
;; (defun local-set-minor-mode-key (mode key def)
;;   "Overrides a minor MODE keybinding KEY DEF for the local buffer.
;;
;;    by creating or altering keymaps stored in buffer-local
;;    `minor-mode-overriding-map-alist'.
;;    Usage example: (local-set-minor-mode-key '<minor-mode> (kbd 'key-to-hide') nil)"
;;   (let* ((oldmap (cdr (assoc mode minor-mode-map-alist)))
;;          (newmap (or (cdr (assoc mode minor-mode-overriding-map-alist))
;;                      (let ((map (make-sparse-keymap)))
;;                        (set-keymap-parent map oldmap)
;;                        (push `(,mode . ,map) minor-mode-overriding-map-alist)
;;                        map))))
;;     (define-key newmap key def)))
;;; ---------------------------------------------------

;;; Solution 2.
;; ----------------------------------------------------------------------------------
;; (add-hook '<major-mode>-hook
;;           (lambda ()
;;             (let ((oldmap (cdr (assoc '<minor-mode> minor-mode-map-alist)))
;;                   (newmap (make-sparse-keymap)))
;;               (set-keymap-parent newmap oldmap)
;;               (define-key newmap [<thekeyIwanttohide>] nil)
;;               (make-local-variable 'minor-mode-overriding-map-alist)
;;               (push `(<minor-mode> . ,newmap) minor-mode-overriding-map-alist))))
;; ----------------------------------------------------------------------------------


;;; [ redefine a map's keybinding buffer-locally ]





(provide 'init-my-emacs-keybinding)

;;; init-my-emacs-keybinding.el ends here
