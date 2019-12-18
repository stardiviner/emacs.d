;;; init-emacs-session.el --- init for Emacs Session.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; Usage:

;; - `recover-session' :: recover session.


;;; Code:

;;; [ desktop ] -- save partial status of Emacs when killed for persistence.

(use-package desktop
  :if (not (featurep 'esup))
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
  :defer 1
  :commands (save-place-mode))

;;; open mostly used files
;; (defun my/open-frequent-used-files ()
;;   "Open my frequently used files."
;;   (find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Programming/Emacs/Data/Emacs Packages/Emacs Packages.org")))
;;   (find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Softwares/Softwares.org")))
;;   (find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Softwares/Commands.org")))
;;   (find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Programming/Implementations/Implementations.org")))
;;   (find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Programming/Implementations/Web/Web Technologies/Web Technologies.org")))
;;   ;; (find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Programming/Emacs/Data/Emacs Packages/Org mode/Org mode.org")))
;;   (find-file (expand-file-name (concat org-directory "/dotfiles/dotfiles.org")))
;;   (find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Programming/Programming Languages/Clojure/Clojure.org")))
;;   (find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Programming/Programming Languages/Clojure/Data/Manuals/My Clojure Language Syntax Reference/My Clojure Language Syntax Reference.org")))
;;   (find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Programming/Programming Languages/Clojure/Data/Manuals/My Clojure Examples/My Clojure Examples.org")))
;;   (find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Programming/Programming Languages/Clojure/Data/Clojure Packages/Clojure Packages.org")))
;;   (find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Programming/Programming Languages/Clojure/ClojureScript/ClojureScript.org")))
;;   ;; (find-file (expand-file-name (concat org-directory "/Wiki/Computer Technology/Programming/Programming Languages/Clojure/ClojureScript/Data/Manuals/My ClojureScript Language Syntax Reference/My ClojureScript Language Syntax Reference.org")))
;;   )
;;
;; ;; (add-hook 'after-init-hook #'my/open-frequent-used-files 'append)
;; (my/open-frequent-used-files)


(provide 'init-emacs-session)

;;; init-emacs-session.el ends here
