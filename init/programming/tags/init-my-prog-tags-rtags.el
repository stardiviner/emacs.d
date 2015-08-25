;;; init-my-prog-tags-rtags.el --- init for rtags
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ rtags ] -- A c/c++ client/server indexer for c/c++/objc[++] with integration for Emacs based on clang.

;;; Usage:
;;
;; - `rtags-imenu'
;; - `rtags-find-symbol'
;; - `rtags-find-references'
;; - `rtags-find-file'
;; - `rtags-find-symbol-at-point'
;; - `rtags-find-references-at-point'
;; - `rtags-find-virtuals-at-point'
;; - `rtags-print-enum-value-at-point'
;; - `rtags-diagnostics'
;; - `rtags-location-stack-back' & `rtags-location-stack-forward'
;; - `rtags-next-match' & `rtags-previous-match', `rtags-jump-to-first-match'
;; - `rtags-print-symbol-info'
;; - `rtags-rename-symbol'
;; - `rtags-reparse-file-if-needed' & `rtags-reparse-file'
;; - `rtags-show-rtags-buffer'

;; Sets up a ton of standard keybindings under C-x r (we try to avoid crashing with the register shortcuts).
;; (rtags-enable-standard-keybindings)


(provide 'init-my-prog-tags-rtags)

;;; init-my-prog-tags-rtags.el ends here
