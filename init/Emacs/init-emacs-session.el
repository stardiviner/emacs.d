;;; init-emacs-session.el --- init for Emacs Session.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; Usage:

;; - `recover-session' :: recover session.


;;; Code:

;;; [ desktop ] -- save partial status of Emacs when killed for persistence.

(use-package desktop
  :if (not (or (featurep 'esup)
               my/emacs-benchmark-toggle))
  :ensure t
  :config
  (setq desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\|^/tmp/\\)")
  (let ((desktop-dir (concat user-emacs-directory ".desktop-save")))
    (unless (file-exists-p desktop-dir)
      (make-directory desktop-dir))
    (add-to-list 'desktop-path desktop-dir))
  (setq desktop-auto-save-timeout (* 60 10))
  (add-hook 'after-init-hook #'desktop-save-mode))

;;; [ saveplace ] -- save visited files' point positions.

(use-package saveplace
  :ensure nil
  :defer 1
  :commands (save-place-mode)
  :hook (after-init . save-place-mode))


(provide 'init-emacs-session)

;;; init-emacs-session.el ends here
