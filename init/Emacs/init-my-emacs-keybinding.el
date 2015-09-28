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

(require 'bind-key)


;;; [ which-key ] -- displays available keybindings in popup.

;; which-key is a minor mode for Emacs that displays the keybindings following
;; your currently entered incomplete command (a prefix) in a popup. For example,
;; after enabling the minor mode if you enter C-x and wait for the default of 1
;; second the minibuffer will expand with all of the available keybindings that
;; follow C-x (or as many as space allows given your settings). This includes
;; prefixes like C-x 8 which are shown in a different face. Screenshots of what
;; the popup will look like are included below. which-key started as a rewrite
;; of guide-key-mode, but the feature sets have diverged to a certain extent.
;;
;; With respect to guide-key, the intention is to provide the following features:
;;
;; - A different polling mechanism to make it lighter on resources than guide-key.
;;
;; - An improved display of keys with more keys being shown by default and a nicer presentation.
;;
;; - Customization options that allow for the rewriting of command names on the
;;   fly through easily modifiable alists.
;;
;; - Good default configurations that work well with most themes.
;;
;; - A well configured back-end for displaying keys (removing the popwin
;;   dependency) that can be easily customized by writing new display functions.
;;
;; Many of these have been implemented and are described below.

(use-package which-key
  :config
  (setq which-key-idle-delay 1.0)

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
                      :foreground "dark cyan" :background " "
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
        '("^helm" "toggle"
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

  (which-key-mode)
  )


;;; [ hydra ] -- be used to tie related commands into a family of short bindings with a common prefix - a Hydra.

;;; Usage:
;;
;; - https://github.com/abo-abo/hydra
;;

;;; simple usages
;;
;; (require 'hydra-examples)
;; (hydra-create "C-M-y" hydra-example-move-window-splitter)
;; (hydra-create "M-g" hydra-example-goto-error)
;; (hydra-create "<f2>" hydra-example-text-scale)

;;; for global bindings
;; (defhydra hydra-zoom (global-map "<f2>")
;;   "zoom"
;;   ("g" text-scale-increase "in")
;;   ("l" text-scale-decrease "out"))

;;; Using Hydra for major-mode or minor-mode bindings
;; (defhydra lispy-vi (lispy-mode-map "C-z")
;;   "vi"
;;   ("l" forward-char)
;;   ("h" backward-char)
;;   ("j" next-line)
;;   ("k" previous-line))

;;; Colorful Hydras
;; Hydra's heads all have a color associated with them:
;;
;; red (default) means the calling this head will not vanquish the Hydra
;; blue means that the Hydra will be vanquished after calling this head
;;
;; (global-set-key
;;  (kbd "C-c C-v")
;;  (defhydra toggle ()
;;    "toggle"
;;    ("a" abbrev-mode "abbrev" :color blue)
;;    ("d" toggle-debug-on-error "debug" :color blue)
;;    ("f" auto-fill-mode "fill" :color blue)
;;    ("t" toggle-truncate-lines "truncate" :color blue)
;;    ("w" whitespace-mode "whitespace" :color blue)
;;    ("q" nil "cancel")))
;; or
;; (global-set-key
;;  (kbd "C-c C-v")
;;  (defhydra toggle (:color blue)
;;    "toggle"
;;    ("a" abbrev-mode "abbrev")
;;    ("d" toggle-debug-on-error "debug")
;;    ("f" auto-fill-mode "fill")
;;    ("t" toggle-truncate-lines "truncate")
;;    ("w" whitespace-mode "whitespace")
;;    ("q" nil "cancel")))

;;; Hydras and numeric arguments
;;
;; - `digit-argment' can be called with 0-9
;; - `negative-argument' can be called with -
;; - `universal-argument' can be called with C-u

;;; Hydras can have `:pre' and `:post' statements
;;
;; (global-set-key
;;  (kbd "C-z")
;;  (defhydra hydra-vi
;;    (:pre
;;     (set-cursor-color "#40e0d0")
;;     :post
;;     (progn
;;       (set-cursor-color "#ffffff")
;;       (message
;;        "Thank you, come again.")))
;;    "vi"
;;    ("l" forward-char)
;;    ("h" backward-char)
;;    ("j" next-line)
;;    ("k" previous-line)
;;    ("q" nil "quit")))

;;; New Hydra color: `amaranth'
;;
;; a new color - amaranth, in addition to the previous red and blue, is
;; available for the Hydra body.
;;
;; According to Wikipedia:
;;
;; The word amaranth comes from the Greek word amaranton, meaning "unwilting"
;; (from the verb marainesthai, meaning "wilt").  The word was applied to
;; amaranth because it did not soon fade and so symbolized immortality.
;;
;; Hydras with amaranth body are impossible to quit with any binding except a
;; blue head. A check for at least one blue head exists in defhydra, so that you
;; don't get stuck by accident.
;;
;; (global-set-key
;;  (kbd "C-z")
;;  (defhydra hydra-vi
;;    (:pre
;;     (set-cursor-color "#40e0d0")
;;     :post
;;     (set-cursor-color "#ffffff")
;;     :color amaranth)
;;    "vi"
;;    ("l" forward-char)
;;    ("h" backward-char)
;;    ("j" next-line)
;;    ("k" previous-line)
;;    ("q" nil "quit")))
;;
;; The only way to exit it, is to press q. No other methods will work. You can
;; use an amaranth Hydra instead of a red one, if for you the cost of being able
;; to exit only though certain bindings is less than the cost of accidentally
;; exiting a red Hydra by pressing the wrong prefix.
;;
;; Note that it does not make sense to define a singe amaranth head, so this
;; color can only be assigned to the body. An amaranth body will always have
;; some amaranth heads and some blue heads (otherwise, it's impossible to exit),
;; no reds.

;;; Embedding sexps in Hydra docstrings
;;
;; (defhydra hydra-marked-items (dired-mode-map "")
;;   "
;; Number of marked items: %(length (dired-get-marked-files))
;; "
;;   ("m" dired-mark "mark"))
;;
;; (define-key dired-mode-map
;;   "m" 'hydra-marked-items/dired-mark)
;;
;;
;;; There's no need for a quitting key, it will auto-vanish when you press
;;; anything other than [m].

;;; Embed variable values into the docstring, among other things.
;;
;; (defhydra hydra-buffer-menu (:color pink)
;;   "
;;   Mark               Unmark             Actions            Search
;; -------------------------------------------------------------------------
;; _m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
;; _s_: save          _U_: unmark up     _b_: bury          _I_: isearch
;; _d_: delete                           _g_: refresh       _O_: multi-occur
;; _D_: delete up                        _T_: files only: %`Buffer-menu-files-only
;; _~_: modified
;; "
;;   ("m" Buffer-menu-mark nil)
;;   ("u" Buffer-menu-unmark nil)
;;   ("U" Buffer-menu-backup-unmark nil)
;;   ("d" Buffer-menu-delete nil)
;;   ("D" Buffer-menu-delete-backwards nil)
;;   ("s" Buffer-menu-save nil)
;;   ("~" Buffer-menu-not-modified nil)
;;   ("x" Buffer-menu-execute nil)
;;   ("b" Buffer-menu-bury nil)
;;   ("g" revert-buffer nil)
;;   ("T" Buffer-menu-toggle-files-only nil)
;;   ("O" Buffer-menu-multi-occur nil :color blue)
;;   ("I" Buffer-menu-isearch-buffers nil :color blue)
;;   ("R" Buffer-menu-isearch-buffers-regexp nil :color blue)
;;   ("c" nil "cancel")
;;   ("v" Buffer-menu-select "select" :color blue)
;;   ("o" Buffer-menu-other-window "other-window" :color blue)
;;   ("q" quit-window "quit" :color blue))
;;
;; (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

(require 'hydra)

;; display a hint with possible bindings in the echo area.
(setq hydra-is-helpful t)

;;; examples
(require 'hydra-examples)

;;; my defined hydras

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


;;; hydra faces
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
