;;; init-my-pm-package.el --- init package.el
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ package.el ] -- Emacs Lisp Package Archive (ELPA)

(require 'cl)

(require 'package)

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
