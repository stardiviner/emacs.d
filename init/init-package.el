;;; init-package.el --- init package.el
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ package.el ] -- Emacs Lisp Package Archive (ELPA)

(require 'package)

(setq package-enable-at-startup nil)

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

(package-initialize)

(add-to-list 'display-buffer-alist
             '("^\\*package-build-result\\*" (display-buffer-reuse-window display-buffer-below-selected)))


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

;; detect whether ~/.emacs.d/elpa/org-9.1.9/ exist?
;; (unless (require 'org nil 'noerror)
;;   (package-install-file (concat user-emacs-directory "init/extensions/org.el")))

(use-package org
  :pin manual
  :load-path "~/Code/Emacs/org-mode/lisp/"
  :preface
  ;; Org Mode modules -- modules that should always be loaded together with org.el.
  ;; t: greedy load all modules.
  ;; nil: disable all extra org-mode modules to speed-up Org-mode file opening.
  (setq org-modules nil)
  :mode (("\\.org\\'" . org-mode))
  :init
  (use-package org-plus-contrib
    :pin manual
    :load-path "~/Code/Emacs/org-mode/contrib/lisp/"
    :no-require t)
  ;; add source code version Org-mode Info into Emacs.
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list "~/Code/Emacs/org-mode/doc/"))
  ;; load org before using some Org settings.
  (require 'org))

;;; [ package-lint ] -- A linting library for elisp package authors.

(use-package package-lint
  :ensure t
  :defer t)

;;; [ flycheck-package ] -- A Flycheck checker for elisp package authors.

(use-package flycheck-package
  :ensure t
  :defer t
  :after flycheck
  :init (flycheck-package-setup))

;;; [ Quelpa ] -- Build and install your Emacs Lisp packages on-the-fly directly from source.

(use-package quelpa
  :ensure t
  :preface (setq quelpa-update-melpa-p nil) ; disable Emacs startup quelpa melpa update.
  :config
  (add-to-list 'quelpa-melpa-recipe-stores
               (concat user-emacs-directory "elpa/recipes")))

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
