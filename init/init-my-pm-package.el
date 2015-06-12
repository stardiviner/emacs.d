;;; init-my-pm-package.el --- init package.el
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ package.el ] -- Emacs Lisp Package Archive (ELPA)

;;; ~/.emacs.d/elpa/

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

(require 'package)
(setq package-archives
      '(("marmalade" . "http://marmalade-repo.org/packages/")
        ("org"       . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ;; ("gnu" . "http://elpa.gnu.org/packages/")
        ))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t) ; t: higher priority source.


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
;; (setq package-enable-at-startup t) ; 

(package-initialize)



(provide 'init-my-pm-package)

;;; init-my-pm-package.el ends here
