;;; init-my-emacs-key-bindings.el --- init Emacs' key bindings.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; stop using the arrow keys
(global-unset-key [left])
(global-unset-key [right])
(global-unset-key [up])
(global-unset-key [down])



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

(setq guide-key/idle-delay 0.1
      ;; guide-key/idle-timer nil ; Idle timer to wait before popping up guide buffer.
      guide-key/polling-time 0.1  ; Polling time to check an input key sequence.
      guide-key/recursive-key-sequence-flag nil
      )

;;; In respect of guide-key/guide-key-sequence, you can add mode specific key
;;; sequences without guide-key/add-local-guide-key-sequence. For example,
;;; configure as below.
(setq guide-key/guide-key-sequence
      '("C-h"                           ; document help lookup
        "C-x r"                         ; register, bookmark, etc
        "C-x 4"                         ; other window
        "C-x 5"                         ; other frame
        "C-x n"                         ; narrow
        "C-c s"                       ; visual-regexp-map
        (dired-mode "C-h"
                    "*" ":" "%"
                    "T" "T >" "T m" "T u"
                    "M-+"
                    "C-t"
                    "M-s"
                    )
        "C-z"                           ; popwin (global)
        ;; "C-c"                           ; extension functional prefix (global)
        "C-c c"                         ; mc (multi-cursor) (global)
        "C-c !"                         ; flycheck (global)
        "C-c &"                         ; yasnippet (global)
        "C-c p"                         ; projectile (global)
        "C-c p 4"                       ; projectile (4: buffer or window)
        "C-c v"                         ; Magit (global)
        "C-c o"                         ; Org-mode (global)
        "C-c d"                         ; Dictionary
        "C-c ;"                         ; E2WM
        "C-c z"                         ; workgroups2
        "C-c @"                         ; hs-minor-mode [Fold] (global)
        "C-c '"                         ;
        (org-mode "C-c C-x" "C-c C-v")  ; Org-mode.
        (outline-minor-mode "C-c @")    ; outline minor mode.
        (markdown-mode "C-c" "C-c C-c" "C-c C-s" "C-c C-t" "C-c TAB" "C-c C-a")
        (ruby-mode "C-h d")             ; Ruby yari mode.
        (rinari-minor-mode "C-c ;" "C-c ; f" "C-c '") ; Rinari minor mode.
        ))


(setq guide-key/highlight-command-regexp "rectangle\\|register\\|bookmark")

;;; change guide-key popup style in popwin.el
(setq guide-key/popup-window-position 'bottom
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


;;; [ global prefix key bindings ]




(provide 'init-my-emacs-key-bindings)

;;; init-my-emacs-key-bindings.el ends here
