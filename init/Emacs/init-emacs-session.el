;;; init-emacs-session.el --- init for Emacs Session.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; Usage:

;; - `recover-session' :: recover session.


;;; Code:

;; [ save-place ]

;; (require 'saveplace)
;; (setq-default save-place t) ; save point place
;; (setq save-place-file (expand-file-name ".places" user-emacs-directory))


;;; [ desktop ] -- save partial status of Emacs when killed for persistence.

(use-package desktop
  :ensure t
  :init
  (let ((desktop-dir (concat user-emacs-directory ".desktop-save")))
    (unless (file-exists-p desktop-dir)
      (make-directory desktop-dir))
    (add-to-list 'desktop-path desktop-dir))
  :config (desktop-save-mode 1)
  (setq desktop-auto-save-timeout (* 60 10)))

;;; open mostly used files
(find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Programming/Emacs/Data/Emacs Packages/Emacs Packages.org")))
(find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Softwares/Softwares.org")))
(find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Softwares/Commands.org")))
(find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Programming/Implementations/Implementations.org")))
(find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Programming/Implementations/Web/Web Technologies/Web Technologies.org")))
(find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Programming/Emacs/Data/Emacs Packages/Org mode/Org mode.org")))
(find-file (expand-file-name (concat org-directory "/dotfiles/dotfiles.org")))
(find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Programming/Programming Languages/Clojure/Clojure.org")))
(find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Programming/Programming Languages/Clojure/ClojureScript/ClojureScript.org")))


(provide 'init-emacs-session)

;;; init-emacs-session.el ends here
