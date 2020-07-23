;;; init-emacs-session.el --- init for Emacs Session.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; Usage:

;; - `recover-session' :: recover session.


;;; Code:

;;; [ desktop ] -- save partial status of Emacs when killed for persistence.

(use-package desktop
  :ensure nil
  :if (not (or (featurep 'esup) my/emacs-benchmark-toggle))
  :config
  (let ((my/desktop-dir (concat user-emacs-directory ".desktop-save")))
    (unless (file-exists-p my/desktop-dir)
      (make-directory my/desktop-dir))
    (add-to-list 'desktop-path my/desktop-dir)
    (setq desktop-dirname my/desktop-dir))
  (desktop-save-mode 1)
  (setq desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\|^/tmp/\\)")
  (setq desktop-auto-save-timeout (* 60 10)))


(provide 'init-emacs-session)

;;; init-emacs-session.el ends here
