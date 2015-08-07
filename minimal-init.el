;;; init.el --- Emacs init file.

;;; Commentary:


;;; Code:

;;; [ package manager ]

;;; el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")


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


