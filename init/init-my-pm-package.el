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
                ))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))


;;; Add support to package.el for pre-filtering available packages
;; (defvar package-filter-function nil
;;   "Optional predicate function used to internally filter packages used by package.el.

;; The function is called with the arguments PACKAGE VERSION ARCHIVE, where
;; PACKAGE is a symbol, VERSION is a vector as produced by `version-to-list', and
;; ARCHIVE is the string name of the package archive.")
;;
;; (defadvice package--add-to-archive-contents
;;   (around filter-packages (package archive) activate)
;;   "Add filtering of available packages using `package-filter-function', if non-nil."
;;   (when (or (null package-filter-function)
;; 	    (funcall package-filter-function
;; 		     (car package)
;; 		     (funcall (if (fboundp 'package-desc-version)
;; 				  'package--ac-desc-version
;; 				'package-desc-vers)
;; 			      (cdr package))
;; 		     archive))
;;     ad-do-it))



;; But don't take Melpa versions of certain packages
;; (setq package-filter-function
;;       (lambda (package version archive)
;;         (or (not (string-equal archive "melpa"))
;;             (not (memq package '())))))



;;; auto refresh packages list if not exist.

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


(provide 'init-my-pm-package)

;;; init-my-pm-package.el ends here
