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
              '(("org"   . "http://orgmode.org/elpa/")
                ("melpa" . "http://melpa.org/packages/")
                ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
                ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                ;; ("gnu" . "https://elpa.gnu.org/packages/")
                ))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))


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


;;; [ flycheck-package ]

(use-package flycheck-package
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-package-setup))
  )


;;; [ org-commentary ] -- generate/update conventional library headers using Org-mode.

(use-package org-commentary
  :ensure t)


(provide 'init-my-pm-package)

;;; init-my-pm-package.el ends here
