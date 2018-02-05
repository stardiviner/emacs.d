;;; init-package.el --- init package.el
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ package.el ] -- Emacs Lisp Package Archive (ELPA)

(require 'cl)

(require 'package)

(setq package-menu-async t)

(setq package-user-dir "~/.emacs.d/elpa")

(defvar elpa-gnu '("gnu" . "https://elpa.gnu.org/packages/"))
;; (defvar elpa-china '("elpa-china" . "http://elpa.emacs-china.org"))
(defvar elpa-melpa '("melpa" . "http://melpa.org/packages/"))
(defvar elpa-melpa-stable '("melpa-stable" . "http://stable.melpa.org/packages/"))
(defvar elpa-marmalade '("marmalade" . "http://marmalade-repo.org/packages/"))
(defvar elpa-org '("org"   . "http://orgmode.org/elpa/"))

(setq-default package-archives nil)
(add-to-list 'package-archives elpa-org t)
(add-to-list 'package-archives elpa-melpa t)
;; (add-to-list 'package-archives elpa-melpa-stable t)
;; (add-to-list 'package-archives elpa-marmalade t)
(add-to-list 'package-archives elpa-gnu t)
;; (add-to-list 'package-archives elpa-china t)

(let* ((elpa-archives-dir "~/.emacs.d/elpa/archives/")
       (elpa-gnu-archives-dir (concat elpa-archives-dir "gnu"))
       (elpa-melpa-archives-dir (concat elpa-archives-dir "melpa"))
       (elpa-org-archives-dir (concat elpa-archives-dir "org")))
  (unless (and (file-exists-p elpa-gnu-archives-dir)
               (file-exists-p elpa-melpa-archives-dir)
               (file-exists-p elpa-org-archives-dir))
    (package-refresh-contents)))



(add-to-list 'display-buffer-alist
             '("^\\*package-build-result\\*" (display-buffer-below-selected)))

;; (setq package-enable-at-startup nil)
(package-initialize)


;;; Load `use-package' ahead before `package-initialize' for (use-package org :pin manual ...).
;;; [ use-package ]

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)                     ; if you use any `:bind' variant

(setq use-package-verbose t ; 'debug: any evaluation errors report to `*use-package*` buffer.
      use-package-always-ensure nil)

(use-package use-package-ensure-system-package
  :ensure t
  :config
  ;; for system-packages
  (setq system-packages-noconfirm t)
  )

;;; NOTE: load manual version Org before MELPA Org to fix conflict issue.
;; (use-package org
;;   :preface
;;   ;; remove Emacs built-in org load-path to avoid it been loaded.
;;   ;; rm -rf /usr/local/share/emacs/27.0.50/lisp/org/
;;   ;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-org.el
;;   (progn
;;     (setq my/org-version-select 'dev)
;;     (defvar my/default-share-directory "/usr/local/share/") ; my Emacs installed path.
;;     (defun my/emacs-version ()
;;       (directory-files (concat my/default-share-directory "emacs/") nil "2.*"))
;;     (defvar org-builtin-lisp-directory
;;       (concat my/default-share-directory "emacs/" (car (my/emacs-version)) "/lisp/org")
;;       "Directory containing lisp files for source code compiled version Emacs built-in Org.")
;;     (defvar org-builtin-info-directory
;;       (concat my/default-share-directory "info") ; TODO: how to only filter out the `org.info.gz'.
;;       "Directory containing Info manual file for source code compiled version Emacs built-in Org.")
;;     (with-eval-after-load 'package
;;       ;; If `my/org-version-select' is *not* `emacs', remove the Emacs version of Org
;;       ;; from the `load-path'.
;;       (unless (eq my/org-version-select 'emacs)
;; 	      ;; Remove Org that ships with Emacs from the `load-path'.
;; 	      (dolist (path load-path)
;;           (when (string-match-p org-builtin-lisp-directory path)
;;             (setq load-path (delete path load-path)))))
;;       ;; If `my/org-version-select' is *not* `elpa', remove the ELPA
;;       ;; version of Org from the `load-path'.
;;       (unless (eq my/org-version-select 'elpa)
;;         (dolist (org-elpa-install-path (directory-files-recursively
;;                                         package-user-dir
;;                                         "\\`org\\(-plus-contrib\\)*-[0-9.]+\\'"
;;                                         :include-directories))
;;           (setq load-path (delete org-elpa-install-path load-path))
;;           ;; Also ensure that the associated path is removed from Info
;;           ;; search list.
;;           (setq Info-directory-list (delete org-elpa-install-path
;; 						                                Info-directory-list))))
;;       ;; if `my/org-version-select' is `dev', let `use-package' to load it.
;;       )
;;     )
;;   ;; specify your own source code version org-mode.
;;   :load-path "~/Code/Emacs/org-mode/lisp/"
;;   :pin manual
;;   :mode (("\\.org$" . org-mode))
;;   :config
;;   (use-package org-plus-contrib
;;     :load-path "~/Code/Emacs/org-mode/contrib/lisp/"
;;     :no-require t
;;     :pin manual)
;;   ;; add source code version Org-mode Info into Emacs.
;;   (with-eval-after-load 'info
;;     (info-initialize)
;;     (add-to-list 'Info-directory-list
;;                  "~/Code/Emacs/org-mode/doc/"))
;;   )


;;; initialize installed packages after (use-package <package> :pin manual ..)
;; (package-initialize)


;;; [ package-lint ] -- A linting library for elisp package authors.

(use-package package-lint
  :ensure t)

;;; [ flycheck-package ] -- A Flycheck checker for elisp package authors.

(use-package flycheck-package
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-package-setup))
  )

;;; [ Quelpa ] -- Build and install your Emacs Lisp packages on-the-fly directly from source.

(use-package quelpa
  :ensure t
  :config
  (setq quelpa-update-melpa-p nil
        ;; quelpa-upgrade-p t
        )

  (add-to-list 'quelpa-melpa-recipe-stores
               (concat user-emacs-directory "elpa/recipes"))
  )

;;; [ Quelpa-use-package ] -- Emacs quelpa handler for use-package.

;; (quelpa
;;  '(quelpa-use-package
;;    :fetcher github
;;    :repo "quelpa/quelpa-use-package"))
;; (require 'quelpa-use-package)

(use-package quelpa-use-package
  :ensure t)



(provide 'init-package)

;;; init-package.el ends here
