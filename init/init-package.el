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
(defvar elpa-melpa '("melpa" . "http://melpa.org/packages/"))
(defvar elpa-melpa-stable '("melpa-stable" . "http://stable.melpa.org/packages/"))
(defvar elpa-marmalade '("marmalade" . "http://marmalade-repo.org/packages/"))
(defvar elpa-org '("org"   . "http://orgmode.org/elpa/"))

(setq-default package-archives nil)
;; (add-to-list 'package-archives elpa-org t)
(add-to-list 'package-archives elpa-melpa t)
;; (add-to-list 'package-archives elpa-melpa-stable t)
;; (add-to-list 'package-archives elpa-marmalade t)
(add-to-list 'package-archives elpa-gnu t)

(package-initialize)

(let* ((elpa-archives-dir "~/.emacs.d/elpa/archives/")
       (elpa-gnu-archives-dir (concat elpa-archives-dir "gnu"))
       (elpa-melpa-archives-dir (concat elpa-archives-dir "melpa"))
       (elpa-org-archives-dir (concat elpa-archives-dir "org")))
  (unless (and (file-exists-p elpa-gnu-archives-dir)
               (file-exists-p elpa-melpa-archives-dir)
               (file-exists-p elpa-org-archives-dir))
    (package-refresh-contents)))

;; (setq package-enable-at-startup nil)
;; (package-initialize nil)


;;; [ use-package ]

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                     ; if you use `:diminish'
(require 'bind-key)                     ; if you use any `:bind' variant

(setq use-package-verbose t
      use-package-always-ensure nil
      use-package-enable-imenu-support t)

(unless (and (package-installed-p 'let-alist)
             (package-installed-p 'seq)
             (package-installed-p 'queue)
             (package-installed-p 'rainbow-mode)
             (package-installed-p 'spinner)
             (package-installed-p 'auctex)
	     (package-installed-p 'bug-hunter)
             )
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
  (package-refresh-contents)
  
  (use-package let-alist :ensure t)
  (use-package seq :ensure t)
  (use-package queue :ensure t)
  (use-package rainbow-mode :ensure t)
  (use-package spinner :ensure t)
  (package-install 'auctex)
  (use-package bug-hunter :ensure t)
  )

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
               (concat user-emacs-directory "melpa/recipes"))
  )

;;; [ Quelpa-use-package ] -- Emacs quelpa handler for use-package.

;; (quelpa
;;  '(quelpa-use-package
;;    :fetcher github
;;    :repo "quelpa/quelpa-use-package"))
;; (require 'quelpa-use-package)

(use-package quelpa-use-package
  :ensure t)

;;; [ Org-mode ]

;;; latest version

(use-package org
  :load-path "~/Code/Emacs/org-mode/lisp/"
  :pin manual
  ;; :quelpa (org :fetcher git :repo "~/Code/Emacs/org-mode/lisp/")
  :mode (("\\.org$" . org-mode))
  :config
  (load "~/Code/Emacs/org-mode/lisp/org.el")
  )

(use-package org-plus-contrib
  :load-path "~/Code/Emacs/org-mode/contrib/lisp/"
  :pin manual
  ;; :quelpa (org-plus-contrib :fetcher git :repo "~/Code/Emacs/org-mode/contrib/lisp/")
  :config
  )



(provide 'init-package)

;;; init-package.el ends here
