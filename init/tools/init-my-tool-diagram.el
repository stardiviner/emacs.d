;;; init-my-tool-diagram.el --- init Emacs diagram tools
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; artist-mode


;;; ditaa


;;; graphviz-dot-mode

;;; Usage:
;; - [C-c c] :: compile
;; - [C-c p] :: preview graph
;; - [M-;] :: 注释或者取消注释

;; (unless (package-installed-p 'graphviz-dot-mode)
;;   (package-install 'graphviz-dot-mode))

;; (load "graphviz-dot-mode.el" nil t t)

;; (add-hook 'find-file-hook (lambda()
;;                             (if (string= "dot" (file-name-extension
;;                                                 buffer-file-name))
;;                                 (progn
;;                                   (message "Enabling Setings for dot-mode")
;;                                   (setq fill-column 1000)
;;                                   (base-auto-pair)
;;                                   (local-set-key (kbd "<C-f6>") 'compile)
;;                                   )
;;                               )))





(provide 'init-my-tool-diagram)

;;; init-my-tool-diagram.el ends here
