;;; init-my-pm-package.el --- init package.el
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ package.el ] -- Emacs Lisp Package Archive (ELPA)

(require 'cl)

(require 'package)

(setq package-menu-async t)

(setq package-user-dir "~/.emacs.d/elpa")

(setq-default package-archives
              '(("melpa" . "http://melpa.org/packages/")
                ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
                ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                ;; ("gnu" . "https://elpa.gnu.org/packages/")
                ("org"   . "http://orgmode.org/elpa/")
                ))

;; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

(package-initialize)

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
      use-package-always-ensure nil)

(unless (and (package-installed-p 'let-alist)
             (package-installed-p 'seq)
             (package-installed-p 'queue)
             (package-installed-p 'rainbow-mode)
             (package-installed-p 'spinner)
             (package-installed-p 'auctex)
             )
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
  (package-refresh-contents)
  
  (use-package let-alist :ensure t)
  (use-package seq :ensure t)
  (use-package queue :ensure t)
  (use-package rainbow-mode :ensure t)
  (use-package spinner :ensure t)
  (use-package auctex :ensure t)
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
  ;; (setq quelpa-upgrade-p t)

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


(provide 'init-my-pm-package)

;;; init-my-pm-package.el ends here
