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


;;; [ skeleton ]




(provide 'init-my-prog-template)

;;; init-my-prog-template.el ends here
