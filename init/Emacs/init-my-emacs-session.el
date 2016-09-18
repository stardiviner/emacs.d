;;; init-my-emacs-session.el --- init for Emacs Session.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; [ save-place ]

;; (require 'saveplace)
;;
;; (setq save-place t                      ; save point place
;;       save-place-file "~/.emacs.d/.emacs-places")


;;; [ desktop ] -- save partial status of Emacs when killed.

(use-package desktop
  :ensure t
  :config
  (setq desktop-path (list (concat user-emacs-directory ".desktop-save")))

  (desktop-save-mode 1)
  )


;;; [ ElScreen ] -- you can have multiple screens (window-configuration).

(use-package elscreen
  :ensure t
  :config
  (setq elscreen-display-screen-number t
        elscreen-display-tab t
        elscreen-tab-display-control nil
        elscreen-tab-display-kill-screen nil
        )
  
  (elscreen-start)
  )


(provide 'init-my-emacs-session)

;;; init-my-emacs-session.el ends here
