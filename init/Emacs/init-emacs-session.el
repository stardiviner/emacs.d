;;; init-emacs-session.el --- init for Emacs Session.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; Usage:

;; - `recover-session' :: recover session.


;;; Code:

;;; [ desktop ] -- save partial status of Emacs when killed for persistence.

(use-package desktop
  :ensure nil
  :preface (let ((my/desktop-dir (expand-file-name "desktop-save" user-emacs-directory)))
             (unless (file-exists-p my/desktop-dir)
               (make-directory my/desktop-dir)))
  :custom ((desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\|^/tmp/\\|(ssh)$\\)")
           (desktop-auto-save-timeout (* 60 10)))
  :config
  (add-to-list 'desktop-path (expand-file-name "desktop-save" user-emacs-directory))
  (desktop-save-mode 1))

;;; [ burly ] -- Save and restore window configurations and their buffers.

;; (use-package burly
;;   :ensure t
;;   :defer t
;;   :commands (burly-bookmark-frames burly-bookmark-windows burly-open-bookmark burly-open-url))


(provide 'init-emacs-session)

;;; init-emacs-session.el ends here
