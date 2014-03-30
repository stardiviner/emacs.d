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




(provide 'init-my-emacs-file)
