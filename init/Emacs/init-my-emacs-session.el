;;; init-my-emacs-session.el --- init for Emacs Session.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ session ]

;;; save-desktop
;;; save-place


;;; [ desktop-save-mode ]

;;; Usage:
;; - commands prefix with `desktop-'.

;; (require 'desktop)


;; save-place

(require 'saveplace)

(setq save-place t                      ; save point place
      save-place-file "~/.emacs.d/.emacs-places")


;;; [ workgroups2 ] --

;;; Usage
;;;
;;; Workgroups is a session manager for Emacs.
;;;
;;;     It saves all your opened buffers, their location and sizes on disk to restore later
;;;     You can create several workspaces
;;;
;;; You can also restore such buffers as: org-agenda, shell, magit-status, help.

(use-package workgroups2
  :ensure t
  :config
  ;; Change prefix key (before activating WG)
  (setq wg-prefix-key (kbd "C-c w"))
  ;; Change workgroups session file
  (setq wg-default-session-file "~/.emacs.d/.emacs_workgroups")

  (setq wg-mode-line-display-on nil         ; toggle workgroups' mode-line display.
        wg-mode-line-disable t            ; do not modify mode-line.
        wg-mode-line-only-name nil          ; only show workgroup name.
        wg-mode-line-use-faces t          ;
        wg-mode-line-decor-divider ":"
        wg-mode-line-decor-left-brace "("
        wg-mode-line-decor-right-brace ")"
        wg-mode-line-decor-workgroup-unmodified #("-" 0 1
                                                  (help-echo "The current workgroup is unmodified"))
        wg-mode-line-decor-workgroup-modified #("*" 0 1
                                                (help-echo "The current workgroup is modified"))
        wg-mode-line-decor-session-modified #("*" 0 1
                                              (help-echo "The session is modified"))
        wg-mode-line-decor-session-unmodified #("-" 0 1
                                                (help-echo "The session is unmodified"))
        wg-mode-line-decor-window-dedicated #("#" 0 1
                                              (help-echo "This window is dedicated to its buffer."))
        wg-mode-line-decor-window-undedicated #("-" 0 1
                                                (help-echo "This window is not dedicated to its buffer."))
        )

  ;; save/restore frame positions
  (setq wg-control-frames t
        wg-restore-frame-position t
        wg-remember-frame-for-each-wg t
        )

  (define-key wg-prefixed-map (kbd "\\") nil)
  (define-key wg-prefixed-map (kbd "-") 'wg-reverse-frame-vertically)

  ;; (workgroups-mode 1)        ; put this one at the bottom of .emacs
  )


(provide 'init-my-emacs-session)

;;; init-my-emacs-session.el ends here
