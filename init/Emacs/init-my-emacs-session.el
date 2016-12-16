;;; init-my-emacs-session.el --- init for Emacs Session.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; Usage:

;; - `recover-session' :: recover session.


;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [ save-place ]

;; (require 'saveplace)
;;
;; (setq save-place t                      ; save point place
;;       save-place-file "~/.emacs.d/.emacs-places")


;;; [ desktop ] -- save partial status of Emacs when killed.

(use-package desktop
  :ensure t
  :init
  (setq desktop-path (list (concat user-emacs-directory ".desktop-save")))

  (desktop-save-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-my-emacs-session)

;;; init-my-emacs-session.el ends here
