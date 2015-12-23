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

;;; user defined tempo snippets
(tempo-define-template "chris"
                       '(> "(lambda (" p ")" n> r> ")">)
                       nil
                       "Insert a template for an anonymous procedure (lambda)")

;; TODO: (define-key company-active-map (kbd "M-k") 'company-tempo-insert)




;;; [ tempo-snippets ]


;;; [ auto-insert ]

(use-package autoinsert
  :config
  (setq auto-insert t)
  (setq auto-insert-directory "~/.emacs.d/templates/")

  (define-auto-insert '("^build\\.xml\\'" . "Java Ant compile file") "build.xml")

  (add-hook 'find-file-hook 'auto-insert)
  )


;;; [ skeleton ]


;;; [ yatemplate ] -- with YASnippet + auto-insert-mode.

(use-package yatemplate
  :ensure t
  :defer t
  :config
  (setq yatemplate-dir (locate-user-emacs-file "templates"))
  )


(provide 'init-my-prog-template)

;;; init-my-prog-template.el ends here
