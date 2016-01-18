;;; init-my-emacs-keybinding.el --- init Emacs' keybinding.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

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

(define-key my-search-prefix (kbd "K") 'search-keybind)


;;; stop using the arrow keys
(global-unset-key [left])
(global-unset-key [right])
(global-unset-key [up])
(global-unset-key [down])


;;; bind some useful commands to keybindings.
(global-set-key (kbd "M-]") 'forward-sentence)
(global-set-key (kbd "M-[") 'backward-sentence)


;;; [ bind-key ] -- A simple way to manage personal keybindings --- IN ATTIC BECAUSE: part of use-package.

;;; Usage:
;; - (bind-key "C-c x" 'my-ctrl-c-x-command)
;;
;;   normal bind a key to a command.
;;
;; - (bind-key* "<C-return>" 'other-window)
;;
;;   If you want the keybinding to override all minor modes that may also bind
;;   the same key, use the bind-key* form.
;;
;; - (bind-key "C-c x" 'my-ctrl-c-x-command some-other-mode-map)
;;
;;   If you want to rebind a key only for a particular mode.
;;
;; - (unbind-key "C-c x" some-other-mode-map)
;;
;;   To unbind a key within a keymap (for example, to stop your favorite major
;;   mode from changing a binding that you don't want to override everywhere),
;;   use unbind-key.
;;
;; - [M-x describe-personal-keybindings]
;;
;;   After Emacs loads, you can see a summary of all your personal keybindings
;;   currently in effect with this command: It will tell you if you've overriden
;;   a default keybinding, and what that default was. Also, it will tell you if
;;   the key was rebound after your binding it with bind-key, and what it was
;;   rebound it to.

(use-package bind-key
  ;; :ensure t
  )


;;; [ which-key ]

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 1.5)

  ;; (setq which-key-popup-type 'side-window ; 'side-window, 'minibuffer, 'frame
  ;;       which-key-side-window-location 'bottom
  ;;       which-key-side-window-max-height 0.33
  ;;       which-key-side-window-max-width 0.25)

  ;; (setq max-mini-window-height 0.25)

  (setq which-key-max-description-length 27
        which-key-separator " ➜ " ; ⇢
        ;; which-key-unicode-correction 3
        which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL")
        ;; which-key-prefix-title-alist '()
        which-key-show-prefix 'echo ; 'left 'echo
        which-key-show-remaining-keys t
        )

  (set-face-attribute 'which-key-key-face nil
                      :foreground "cyan"
                      )
  (set-face-attribute 'which-key-special-key-face nil
                      :inverse-video t
                      )
  (set-face-attribute 'which-key-note-face nil
                      :foreground "white"
                      )
  (set-face-attribute 'which-key-separator-face nil
                      :foreground "#444444"
                      )
  (set-face-attribute 'which-key-command-description-face nil
                      :foreground "dark cyan"
                      )
  (set-face-attribute 'which-key-group-description-face nil
                      :foreground "orange red" :background "#333333"
                      )
  (set-face-attribute 'which-key-local-map-description-face nil
                      :foreground "yellow"
                      :slant 'italic
                      )
  (set-face-attribute 'which-key-highlighted-command-face nil
                      :inherit 'which-key-command-description-face
                      :underline "dark red")

  (setq which-key-highlighted-command-list
        '("toggle"
          "register" "bookmark"
          "rectangle" "iedit"
          ("emacs" . highlight)
          ))
  
  ;; (setq which-key-key-based-description-replacement-alist
  ;;       '(("C-x C-f" . "find files")
  ;;         (org-mode . (("C-c C-c" . "Org C-c C-c")
  ;;                      ("C-c C-a" . "Org Attach")))
  ;;         ))
  ;;
  ;; (setq which-key-sort-order 'which-key-key-order)
  ;;
  ;; `which-key-show-next-page'
  ;; (setq which-key-use-C-h-for-paging nil)
  ;; (define-key which-key-mode-map (kbd "C-x <f5>") 'which-key-show-next-page) ; custom
  ;; equivalent to =
  ;; (setq which-key-paging-prefixes '("C-x"))
  ;; (setq which-key-paging-key "<f5>")
  
  ;; (which-key-setup-minibuffer)
  ;; (which-key-setup-side-window-bottom) ; default
  ;; (which-key-setup-side-window-right)
  ;; (which-key-setup-side-window-right-bottom)

  (global-set-key (kbd "C-x C-t") 'which-key-show-top-level)

  (which-key-mode)
  )


;;; [ hydra ] -- tie related commands into a family of short bindings with a common prefix - a Hydra.

(use-package hydra
  :ensure t
  :config
  ;; display a hint with possible bindings in the echo area.
  (setq hydra-is-helpful t)

  ;; examples
  (require 'hydra-examples)

  ;; hydra faces
  (set-face-attribute 'hydra-face-red nil
                      :foreground "red"
                      :weight 'bold)
  (set-face-attribute 'hydra-face-blue nil
                      :foreground "blue"
                      :weight 'bold)
  (set-face-attribute 'hydra-face-amaranth nil
                      :foreground "orange"
                      :weight 'bold)
  (set-face-attribute 'hydra-face-pink nil
                      :foreground "pink"
                      :weight 'bold)
  (set-face-attribute 'hydra-face-teal nil
                      :foreground "cyan"
                      :weight 'bold)
  
  ;; my defined hydras

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
     ("t" transpose-frame "'" :color blue)
     ("f" my-turn-current-window-into-new-frame "frame" :color blue)
     ("o" delete-other-windows "one" :color red)
     ("a" ace-window "ace" :color cyan)
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

  ;; This means that when I press < from the start of the line, a Hydra will be
  ;; called instead of inserting <, otherwise < will be inserted.

  ;; (defhydra hydra-org-template (:color red :hint nil)
  ;;   "
  ;; _c_enter  _q_uote    _L_aTeX:
  ;; _l_atex   _e_xample  _i_ndex:
  ;; _a_scii   _v_erse    _I_NCLUDE:
  ;; _s_rc     ^ ^        _H_TML:
  ;; _h_tml    ^ ^        _A_SCII:
  ;; "
  ;;   ("s" (hot-expand "<s"))
  ;;   ("e" (hot-expand "<e"))
  ;;   ("q" (hot-expand "<q"))
  ;;   ("v" (hot-expand "<v"))
  ;;   ("c" (hot-expand "<c"))
  ;;   ("l" (hot-expand "<l"))
  ;;   ("h" (hot-expand "<h"))
  ;;   ("a" (hot-expand "<a"))
  ;;   ("L" (hot-expand "<L"))
  ;;   ("i" (hot-expand "<i"))
  ;;   ("I" (hot-expand "<I"))
  ;;   ("H" (hot-expand "<H"))
  ;;   ("A" (hot-expand "<A"))
  ;;   ("<" self-insert-command "ins")
  ;;   ("o" nil "quit"))
  ;;
  ;; (require 'org)
  ;; (defun hot-expand (str)
  ;;   "Expand org template."
  ;;   (insert str)
  ;;   (org-try-structure-completion))
  ;;  
  ;; (define-key org-mode-map "<"
  ;;   (lambda () (interactive)
  ;;     (if (looking-back "^")
  ;;         (hydra-org-template/body)
  ;;       (self-insert-command 1))))
  
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

;; avoid always select unwanted first candidate in ac when in Org writing.
;; FIXME: this set to nil can not work `locally'.
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (eval-after-load 'auto-complete
;;               (define-key ac-menu-map (kbd "SPC") 'self-insert-command))
;;            
;;             ;; *locally* unset/unbind/undefine keybinding.
;;             ;; X (define-key ac-menu-map (kbd "SPC") nil)
;;             ;; X (define-key ac-menu-map (kbd "SPC") 'self-insert-command)
;;             ;; X (setq-local ac-menu-map (delq (kbd "SPC") ac-menu-map))
;;             ))




(provide 'init-my-emacs-keybinding)

;;; init-my-emacs-keybinding.el ends here
