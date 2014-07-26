;;; init-my-prog-lang-java.el --- init Java
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ javadoc-lookup ]

;;; Usage:
;; - [M-x javadoc-lookup] -- [C-h d]
;;
;;; Import functions
;;
;; Two functions for managing Java imports is provided: `add-java-import' and
;; `sort-java-imports'. The former integrates with the javadoc-lookup index to
;; provide completions.

(define-key java-mode-map (kbd "C-h d") 'javadoc-lookup)

(javadoc-add-roots "/usr/share/doc/openjdk-7-doc/api"
                   ;; "~/src/project/doc"
                   )


;;; [ javadoc-help ]


;;; [ java-complete ]



(provide 'init-my-prog-lang-java.el)

;;; init-my-prog-lang-java.el.el ends here
