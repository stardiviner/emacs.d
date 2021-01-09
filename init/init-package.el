;;; init-package.el --- init package.el
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ package.el ] -- Emacs Lisp Package Archive (ELPA)

(require 'package)

(setq package-enable-at-startup nil)

(setq package-menu-async t)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;;; ELPA Mirrors
;; (setq-default package-archives
;; 	            '(("gnu" . "https://elpa.gnu.org/packages/")
;; 		            ("melpa" . "http://melpa.org/packages/")
;; 		            ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
;; 		            ;; ("marmalade" . "http://marmalade-repo.org/packages/")
;; 		            ("org"   . "http://orgmode.org/elpa/")))

(setq-default package-archives
              '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))

(let* ((elpa-archives-dir (expand-file-name "elpa/archives/" user-emacs-directory))
       (elpa-gnu-archives-dir (concat elpa-archives-dir "gnu"))
       (elpa-melpa-archives-dir (concat elpa-archives-dir "melpa"))
       (elpa-org-archives-dir (concat elpa-archives-dir "org")))
  (unless (and (file-exists-p elpa-gnu-archives-dir)
               (file-exists-p elpa-melpa-archives-dir)
               (file-exists-p elpa-org-archives-dir))
    (package-refresh-contents)))

(package-initialize)

(add-to-list 'display-buffer-alist '("^\\*package-build-result\\*" . (display-buffer-below-selected)))


;;; Load `use-package' ahead before `package-initialize' for (use-package org :pin manual ...).
;;; [ use-package ]

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)                     ; if you use any `:bind' variant
(use-package delight                    ; if you use `:delight'
  :ensure t)
;; (use-package deminish                   ; if you use `:diminish'
;;   :ensure t)

(setq use-package-verbose t ; 'debug: any evaluation errors report to `*use-package*` buffer.
      use-package-always-ensure nil)

(if (not (file-exists-p "~/Code/Emacs/org-mode/lisp/"))
    (use-package org
      :pin org
      :ensure t
      :ensure org-plus-contrib
      :mode (("\\.org\\'" . org-mode)))
  
  ;; disable Emacs built-in Org Mode
  (delete (format "/usr/local/share/emacs/%s/lisp/org" emacs-version) load-path)
  (delete "/usr/share/emacs/site-lisp/org/" load-path)
  (use-package org
    :pin manual
    :load-path "~/Code/Emacs/org-mode/lisp/"
    :defer t
    :mode (("\\.org\\'" . org-mode))
    ;; load org before org-mode init files settings.
    :init (require 'org)
    ;; add source code version Org-mode Info into Emacs.
    (if (file-exists-p "~/Code/Emacs/org-mode/doc/org")
        (with-eval-after-load 'info
          (add-to-list 'Info-directory-list "~/Code/Emacs/org-mode/doc/")
          (info-initialize)))
    (use-package org-plus-contrib
      :pin manual
      :load-path "~/Code/Emacs/org-mode/contrib/lisp"
      :no-require t)))

(setq org-modules nil) ; disable all extra org-mode modules to speed-up Org-mode file opening.
(setq org-startup-folded t)
(setq org-agenda-inhibit-startup t)

;;; [ package-lint ] -- A linting library for elisp package authors.

(use-package package-lint
  :ensure t
  :defer t
  :init (add-to-list 'display-buffer-alist '("^\\*Package-Lint\\*\\'" . (display-buffer-below-selected))))

;;; [ flycheck-package ] -- A Flycheck checker for elisp package authors.

(use-package flycheck-package
  :ensure t
  :defer t
  :after flycheck
  :init (flycheck-package-setup))

;;; [ quelpa ]

(use-package quelpa-use-package
  :ensure t
  :demand t
  :commands (quelpa-upgrade quelpa-upgrade-all quelpa-self-upgrade)
  :custom ((quelpa-checkout-melpa-p nil)
           (quelpa-update-melpa-p nil)))



(provide 'init-package)

;;; init-package.el ends here
