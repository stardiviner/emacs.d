;;; init-tool-utilities.el --- init file of utilities -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-06-27 10:58:32 stardiviner>

;;; Commentary:



;;; Code:

;;; [ qr-native ] -- libqr binding of Emacs Lisp.

(use-package qr-native
  ;; :quelpa (qr-native :fetcher github :repo "syohex/emacs-qr-native")
  :load-path "~/Code/Emacs/qr-native"
  :commands (qr-native-display-string))



(provide 'init-tool-utilities)

;;; init-tool-utilities.el ends here
