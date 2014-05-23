;;; init-my-emacs-keybindings.el --- init Emacs' key bindings.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; stop using the arrow keys
(global-unset-key [left])
(global-unset-key [right])
(global-unset-key [up])
(global-unset-key [down])


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

(setq guide-key/idle-delay 1.5 ; longer time can delay guide-key popup to speed up Emacs.
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
        "C-x RET"                       ; coding system
        "C-x C-a"                       ; edebug
        "C-c C-x"                       ; edebug-x
        "C-x *"                         ; calc
        "C-x a"                         ; abbrev
        "C-x n"                         ; narrow
        "C-c r"                         ; regexp prefix map
        "C-c s"                         ; visual-regexp-map
        "C-c c"                         ; mc (multi-cursor) (global)
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
        "C-c SPC"                       ; allout (outline)
        "C-c &"                         ; yasnippet (global)
        "C-c p"                         ; projectile (global)
        "C-c p 4"                       ; projectile (4: buffer or window)
        "C-c d"                         ; Debug
        "C-c v"                         ; Magit (global)
        "C-c g"                         ; Git
        "C-c o"                         ; Org-mode (global)
        "C-x d"                         ; Dictionary
        "C-c ;"                         ; E2WM
        "C-c w"                         ; workgroups2
        "C-c @"                         ; hs-minor-mode [Fold] (global)
        "C-c t"                         ; tools: paste(gist), ... etc.
        "C-x c"                         ; Helm
        "C-x 8"                         ; insert unicode IDE symbols
        ;; "C-c '"                         ; ???
        (org-mode "C-c C-x" "C-c C-v")  ; Org-mode.
        (outline-minor-mode "C-c @")    ; outline minor mode.
        (markdown-mode "C-c" "C-c C-c" "C-c C-s" "C-c C-t" "C-c TAB" "C-c C-a")
        (latex-mode "C-c" "C-c C-p")    ; LaTeX mode.
        (ruby-mode "C-c") ; Ruby yari mode.
        (rinari-minor-mode "C-c ;" "C-c ; f" "C-c '") ; Rinari minor mode.
        (web-mode "C-c")                ; web-mode.
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
