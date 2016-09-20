;;; init-my-prog-template.el --- init for Template
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ tempo ]

;;; Usage:
;;
;; - `tempo-template-???' :: user defined template functions.

(require 'tempo)

(setq tempo-interactive t               ; `tempo-insert' prompt.
      tempo-insert-region t
      ;; tempo-tags
      )


;;; [ tempo-snippets ]


;;; [ auto-insert ]

(use-package autoinsert
  :ensure t
  :config
  ;; (setq auto-insert-query 'function)
  (setq auto-insert-directory (locate-user-emacs-file "templates/"))

  (auto-insert-mode 1)
  
  ;; templates: `auto-insert-alist'
  (setq auto-insert-alist nil)
  (define-auto-insert '("\\.html?$") "default.html")
  (define-auto-insert '("^build\\.xml\\'" . "Java Ant compile file") "build.xml")
  )


;;; [ skeleton ]


;;; [ yatemplate ] -- with YASnippet + auto-insert-mode.

;; (use-package yatemplate
;;   :ensure t
;;   :config
;;   (setq yatemplate-dir (locate-user-emacs-file "templates"))
;;   )


(provide 'init-my-prog-template)

;;; init-my-prog-template.el ends here
