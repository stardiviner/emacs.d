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
(defvar elpa-melpa '("melpa" . "http://melpa.org/packages/"))
(defvar elpa-melpa-stable '("melpa-stable" . "http://stable.melpa.org/packages/"))
(defvar elpa-marmalade '("marmalade" . "http://marmalade-repo.org/packages/"))
(defvar elpa-org '("org"   . "http://orgmode.org/elpa/"))

;;; ELPA Mirrors
(setq-default package-archives
              '(("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                ("org-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))

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
             '("^\\*package-build-result\\*"
               (display-buffer-reuse-window display-buffer-below-selected)))


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

;; detect whether ~/.emacs.d/elpa/org-9.1.9/ exist?
;; (unless (require 'org nil 'noerror)
;;   (package-install-file (concat user-emacs-directory "init/extensions/org.el")))

(use-package org
  :pin org
  :ensure t
  ;; :pin manual
  ;; :load-path "~/Code/Emacs/org-mode/lisp/"
  :defer t
  :preface
  ;; Org Mode modules -- modules that should always be loaded together with org.el.
  ;; t: greedy load all modules.
  ;; nil: disable all extra org-mode modules to speed-up Org-mode file opening.
  (setq org-modules nil)
  :mode (("\\.org\\'" . org-mode))
  :init
  ;; add source code version Org-mode Info into Emacs.
  (with-eval-after-load 'info
    (add-to-list 'Info-directory-list "~/Code/Emacs/org-mode/doc/")
    (info-initialize))
  ;; load org before using some Org settings.
  (require 'org)
  (use-package org-plus-contrib
    :pin org
    :ensure t
    ;; :pin manual
    ;; :load-path "~/Code/Emacs/org-mode/contrib/lisp/"
    :defer t
    :no-require t))

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

;;; [ leaf ] -- Simplify your init.el configuration, extended use-package.

(prog1 "prepare leaf"
  (prog1 "package"
    (custom-set-variables
     '(package-archives '(("org"   . "https://orgmode.org/elpa/")
                          ;; ("melpa" . "https://melpa.org/packages/")
                          ;; ("gnu"   . "https://elpa.gnu.org/packages/")
                          ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                          ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                          )))

    (unless (and package--initialized (not after-init-time))
      (package-initialize)))

  (prog1 "leaf"
    (unless (package-installed-p 'leaf)
      (unless (assoc 'leaf package-archive-contents)
        (package-refresh-contents))
      (condition-case err
          (package-install 'leaf)
        (error
         (package-refresh-contents)       ; renew local melpa cache if fail
         (package-install 'leaf))))

    (leaf leaf-keywords
      :ensure t
      :config (leaf-keywords-init)))

  (prog1 "optional packages for leaf-keywords"
    ;; optional packages if you want to use :hydra, :el-get,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t
      :custom ((el-get-git-shallow-clone  . t)))))



(provide 'init-package)

;;; init-package.el ends here
