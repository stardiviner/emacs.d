;;; init-my-emacs-session.el --- init for Emacs Session.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; Usage:

;; - `recover-session' :: recover session.


;;; Code:

;; [ save-place ]

;; (require 'saveplace)
;; (setq-default save-place t) ; save point place
;; (setq save-place-file (expand-file-name ".places" user-emacs-directory))


;;; [ desktop ] -- save partial status of Emacs when killed.

(use-package desktop
  :ensure t
  :config
  (setq desktop-path (list (concat user-emacs-directory ".desktop-save")))

  (desktop-save-mode 1)
  )

;;; [ eyebrowse ] -- A simple-minded way of managing window configs in Emacs.

(use-package eyebrowse
  :ensure t
  :config
  (setq eyebrowse-new-workspace t)
  
  ;; also save side and slot windows configuration.
  (add-to-list 'window-persistent-parameters '(window-side . writable))
  (add-to-list 'window-persistent-parameters '(window-slot . writable))

  (eyebrowse-mode t)
  )


(provide 'init-my-emacs-session)

;;; init-my-emacs-session.el ends here
