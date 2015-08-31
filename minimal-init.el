;;; init.el --- Emacs init file.

;;; Commentary:


;;; Code:

;;; [ Debug ]

(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit t)


;;; some settings for easy usage

(defalias 'yes-or-no-p 'y-or-n-p)

;;; [ package manager ]

;;; el-get

(add-to-list 'load-path "~/.emacs.d/el-get")


;;; [ package.el ]

;; (require 'package)
;; (setq package-archives
;;       '(("marmalade" . "http://marmalade-repo.org/packages/")
;;         ("org"       . "http://orgmode.org/elpa/")
;;         ("melpa"     . "http://melpa.milkbox.net/packages/")))
;; (package-initialize)


;;; add my init files directory
(let ((default-directory "~/.emacs.d/init/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ; shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))


