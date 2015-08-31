;;; init-my-emacs-file.el --- 
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;;; archive file
(auto-compression-mode 1)           ; auto decompress when open compressed file.


;;; [ DovView mode ]
;; Emacs utility for viewing PDF/PS/DVI/OpenDocument/Microsoft Office documents files in Emacs.
;; Usage:
;; When you visit a document file that can be displayed with DocView mode, Emacs auto use DocView mode.
;; - [M-x doc-view]
;; - `doc-view-clear-cache' :: to delete all cached files.
;; - `doc-view-dired' :: for Dired users.

;; (require 'doc-view)

(autoload 'doc-view "DocView mode" t)

(setq doc-view-conversion-refresh-interval 1)


;;; large file

;;; [ vlf -- View Large Files ]

;;; Emacs minor mode that allows viewing, editing, searching and comparing large
;;; files in batches, trading memory for processor time. Batch size can be
;;; adjusted on the fly and bounds the memory that is to be used for operations
;;; on the file. This way multiple large files (like terabytes or whatever) can
;;; be instantly and simultaneously accessed without swapping and degraded
;;; performance.

;;; Usage:
;;; - [M-x vlf PATH-TO-FILE]
;;; - [C-c C-v] -- prefix.

(require 'vlf)
;; (require 'vlf-integrate)

;;; All VLF operations are grouped under the C-c C-v prefix by default. Here’s
;;; example how to add another prefix (C-x v):
;; (eval-after-load "vlf"
;;   '(define-key vlf-prefix "\C-xv" vlf-mode-map))




(provide 'init-my-emacs-file)

;;; init-my-emacs-file.el ends here
