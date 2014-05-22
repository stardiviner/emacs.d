;;; init-my-emacs-package-management.el ---
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ package.el ] --

;; ;; on-demand installation of a package
;; (defun stardiviner/require-package (package &optional min-version no-refresh)
;;   "Install given PACKAGE, optionally requiring MIN-VERSION.
;; If NO-REFRESH is non-nil, the available package lists will not be
;; re-downloaded in order to locate PACKAGE."
;;   (if (package-installed-p package min-version)
;;       t
;;     (if (or (assoc package package-archive-contents) no-refresh)
;;         (package-install package)
;;       (progn
;;         (package-refresh-contents)
;;         (stardiviner/require-package package min-version t)))))
;;
;; ;; on-demand installation of multiple packages
;; (defun stardiviner/require-packages (packages-list)
;;   "Install packages from a given PACKAGES-LIST, using `stardiviner-require-package' function."
;;   (mapc #'stardiviner/require-package packages-list))

;;; Usage:
;; e.g.
;; (stardiviner/require-package 'auto-complete)


;;; [ el-get ] --

;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))

;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; ;;; custom
;; ;; (setq el-get-sources)

;; (el-get 'sync)


;;; [ Cask ] -- Emacs dependency management made easy

;;; Cask is a tool for managing dependencies in Emacs, both your local Emacs installation and Emacs package development. It's similar to Ruby's Bundler and node's npm.

;; (require 'cask "~/.cask/cask.el")
;; (cask-initialize)



(provide 'init-my-emacs-package-management)

;;; init-my-emacs-package-management.el ends here
