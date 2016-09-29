;;; [ Debug ]
(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)

;;; [ profiler ]
;; (profiler-start 'cpu+mem)

;;; benchmark
;; (require 'init-my-emacs-benchmark)

;;; add my init files directory
;; (let ((default-directory "~/.emacs.d/init/"))
;;   (setq load-path
;;         (append
;;          (let ((load-path (copy-sequence load-path))) ; shadow
;;            (append
;;             (copy-sequence (normal-top-level-add-to-load-path '(".")))
;;             (normal-top-level-add-subdirs-to-load-path)))
;;          load-path)))

;;; [ package.el ]
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

(package-initialize)

;;; [ use-package ]
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; (require 'color)
;; (use-package color-theme
;;   :ensure t)

;;; my custom functions
;; (require 'init-my-library)
;; (require 'init-my-functions)

;;; add your customizations from here

(use-package magithub
  :ensure t)
