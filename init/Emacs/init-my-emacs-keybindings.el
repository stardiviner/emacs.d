;;; init-my-emacs-keybindings.el --- init Emacs' key bindings.
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

(define-key my-search-prefix-map (kbd "K") 'search-keybind)


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



;;; [ guide-key ] -- Guide following keys to an input key sequence automatically and dynamically in Emacs.

;;; Features:
;;;
;;; - guide-key.el displays the available key bindings automatically and
;;;   dynamically. guide-key aims to be an alternative of one-key.el.
;;;
;;; - guide-key can highlight particular commands. This makes it easy to find a
;;;   command you are looking for, and to learn its key binding.
;;;
;;; - guide-key doesn’t overwrite existing commands and key bindings, so there
;;;   is no interference with `describe-key' and `describe-bindings'.

;;; Usage:
;;; - Just like [C-x r ] + [C-h] ::
;;;        to show a list of key bindings which start with [C-x r].
;;; - When you press these prefix keys, key bindings are automatically popped up after a short delay (1 second by default).
;;; - [any key sequence prefix] ::

(require 'guide-key)

(setq guide-key/idle-delay 1.0 ; longer time can delay guide-key popup to speed up Emacs.
      guide-key/idle-timer nil ; Idle timer to wait before popping up guide buffer.
      guide-key/polling-time 0.1  ; Polling time to check an input key sequence.
      guide-key/recursive-key-sequence-flag t ; guide-key checks an input key sequence recursively. the guide buffer is popped up when you input “C-x r”, “C-x 8” and any other prefixes following “C-x”.
      )

;;; In respect of guide-key/guide-key-sequence, you can add mode specific key
;;; sequences without guide-key/add-local-guide-key-sequence. For example,
;;; configure as below.
(setq guide-key/guide-key-sequence
      '("C-h"                           ; document help lookup
        "C-x r"                         ; register, bookmark, etc
        "C-x 4"                         ; other window
        "C-x 5"                         ; other frame
        "C-x C-k"                       ; macro/kmacro
        "C-x RET"                       ; coding system
        "C-x C-a"                       ; edebug
        "C-c C-x"                       ; edebug-x
        "C-x *"                         ; calc
        "C-x a"                         ; abbrev
        "C-x n"                         ; narrow
        "C-c %"                         ; mmm-mode
        "C-c e"                         ; edit (multiple-cursor, narrow, ...)
        "C-c r"                         ; regexp prefix map
        "C-c s"                         ; visual-regexp-map
        "C-c l"                         ; tags lookup
        (dired-mode "C-h"
                    "*" ":" "%"
                    "T" "T >" "T m" "T u"
                    "M-+"
                    "C-t"
                    "M-s"
                    )
        "C-z"                           ; popwin (global)
        ;; "C-c"                           ; extension functional prefix (global)
        "C-c !"                         ; flycheck (global)
        "C-c t"                         ; programming test (global)
        "C-c SPC"                       ; allout (outline)
        "C-c &"                         ; yasnippet (global)
        "C-c p"                         ; projectile (global)
        "C-c p C-r"                     ; projectile-rails
        "C-c C-r"                       ; projectile-rails
        "C-c d"                         ; Debug
        "C-c v"                         ; Magit (global)
        "C-c g"                         ; Git
        "C-c i"                         ; Inferior
        "C-c o"                         ; Org-mode (global)
        "C-x d"                         ; Dictionary
        "C-c ;"                         ; E2WM
        "C-c w"                         ; workgroups2
        "C-x x"                         ; perspective
        "C-c @"                         ; hs-minor-mode [Fold] (global)
        "C-c T"                         ; tools: paste(gist), ... etc.
        "C-x c"                         ; Helm
        "C-x 8"                         ; insert unicode IDE symbols
        ;; "C-c '"                         ; ???
        (org-mode "C-c C-x" "C-c C-v")  ; Org-mode.
        (outline-minor-mode "C-c @")    ; outline minor mode.
        (markdown-mode "C-c" "C-c C-c" "C-c C-s" "C-c C-t" "C-c TAB" "C-c C-a")
        (latex-mode "C-c" "C-c C-p")    ; LaTeX mode.
        (artist-mode "C-c C-a")         ; artist-mode
        (web-mode "C-c")                ; web-mode.
        (ruby-mode "C-c") ; Ruby yari mode.
        (rinari-minor-mode "C-c ;" "C-c '") ; Rinari minor mode.
        ))

(setq guide-key/highlight-command-regexp "rectangle\\|register\\|bookmark")

;;; change guide-key popup style in popwin.el
(setq guide-key/popup-window-position 'bottom
      ;; This variable controls the size of text in guide buffer. The default
      ;; value is 0 (it means default size in Emacs). If you want to enlarge
      ;; text, set positive number. Otherwise, set negative number.
      ;; guide-key/text-scale-amount 0
      )

;;; Add settings in a particular mode
(defun guide-key/my-hook-function-for-org-mode ()
  ;; (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-guide-key-sequence "C-c C-v")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)

;;; Faces
(set-face-attribute 'guide-key/key-face nil
                    :foreground "red")
(set-face-attribute 'guide-key/prefix-command-face nil
                    :foreground "cyan")
(set-face-attribute 'guide-key/highlight-command-face nil
                    :foreground "white")

;;; enable guide-key mode.
(guide-key-mode 1)
(diminish 'guide-key-mode)


;;; [ guide-key-tip ] -- Interface of guide-key.el using pos-tip.el in Emacs

;; (require 'guide-key-tip)
;; (setq guide-key-tip/enabled t)


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

(require 'hydra)

(setq hydra-is-helpful t ; display a hint with possible bindings in the echo area.
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




(provide 'init-my-emacs-keybindings)

;;; init-my-emacs-keybindings.el ends here
