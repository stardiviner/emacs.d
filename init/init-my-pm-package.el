;;; init-my-pm-package.el --- init package.el
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ package.el ] -- Emacs Lisp Package Archive (ELPA)

;;; How Packages work in Emacs 24
;;
;; Whenever Emacs starts up, it automatically calls the function ‘
;; package-initialize’ to load installed packages. This is done after loading
;; the init file and abbrev file (if any) and before running ‘after-init-hook’
;; (see Startup Summary). Automatic package loading is disabled if the user
;; option package-enable-at-startup is nil.

;;; Usage:
;; - [M-x package-list-packages] :: list out packages.
;; - [M-x package-install RET] :: install package.
;;
;; Keep in mind that MELPA packages are built automatically from the master
;; branch, meaning bugs might creep in there from time to time. Never-the-less,
;; installing from MELPA is the recommended way of obtaining CIDER, as the
;; master branch is normally quite stable and "stable" (tagged) builds are
;; released somewhat infrequently.
;;
;; With the most recent builds of Emacs, you can pin CIDER to always use MELPA
;; Stable (or Marmalade) by adding this to your Emacs initialization:
;;
;;   (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(require 'cl)

(require 'package)

(setq package-user-dir "~/.emacs.d/elpa")

(setq-default package-archives
              '(
                ("org"   . "http://orgmode.org/elpa/")
                ("melpa" . "http://melpa.org/packages/")
                ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
                ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                ;; ("gnu" . "http://elpa.gnu.org/packages/")
                ))


;;; Add support to package.el for pre-filtering available packages
(defvar package-filter-function nil
  "Optional predicate function used to internally filter packages used by package.el.

The function is called with the arguments PACKAGE VERSION ARCHIVE, where
PACKAGE is a symbol, VERSION is a vector as produced by `version-to-list', and
ARCHIVE is the string name of the package archive.")

(defadvice package--add-to-archive-contents
  (around filter-packages (package archive) activate)
  "Add filtering of available packages using `package-filter-function', if non-nil."
  (when (or (null package-filter-function)
	    (funcall package-filter-function
		     (car package)
		     (funcall (if (fboundp 'package-desc-version)
				  'package--ac-desc-version
				'package-desc-vers)
			      (cdr package))
		     archive))
    ad-do-it))



;; But don't take Melpa versions of certain packages
(setq package-filter-function
      (lambda (package version archive)
        (or (not (string-equal archive "melpa"))
            (not (memq package '())))))



;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  ;; TODO: if package is not in `my-packages' list, then ignore. (message "package is not in `my-packages' list")
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))



;; (when (null package-archive-contents)
;;   (package-refresh-contents))



;; The packages that you install with package.el are activated by default after
;; your .emacs is loaded. To be able to use them before the end of your .emacs
;; you need to activate them by using the commands:
;;
;; (setq package-enable-at-startup t) ; default

;; This determines which packages should be loaded at start-up.
;; (setq package-load-list  '(all)) ; default


(package-initialize)


;;; [ install & load my packages ]


(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs packages is now refreshing its package database ...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p)))
  )


;;; [ paradox ]

;;; Usage:
;;
;; - [M-x paradox-list-packages]




(provide 'init-my-pm-package)

;;; init-my-pm-package.el ends here
