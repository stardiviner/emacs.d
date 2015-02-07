;;; init-my-prog-lang-database.el --- init Databases settings

;;; Commentary:



;;; Code:

;;; [ sql-indent]
;; (require 'sql-indent)



;;; Auto Uppercase SQL Keywords
;; (unless (not (featurep 'sqlup-mode))
;;   (setq sql-abbrev-file-name "~/.emacs.d/init/abbrevs/sql.abbrev")
;;   (if (file-exists-p sql-abbrev-file-name)
;;       (load sql-abbrev-file-name)))


;;; [ sqlup-mode ]

;;; Usage:
;; - The capitalization is triggered when you press 'SPC', ';', ',', or '(', '\r' (Enter),
;; - [C-c u] region :: `sqlup-capitalize-keywords-in-region'

(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-inretactive-mode-hook 'sqlup-mode)

;; (add-hook 'sqlup-mode-hook
;;           (lambda ()
;;             (setq sqlup-keywords
;;                   (append sqlup-keywords
;;                           '("text" "glob" "offset")))))


;;; [ edbi ]

;;; Usage:
;;
;; - M-x `edbi:open-db-viewer' opens a dialog for DB connection.
;;
;; Check the key-bind `edbi:dbview-keymap'.
;;
;;     j,k, n,p : navigation for rows
;;     c : switch to query editor buffer
;;     RET : show table data
;;     SPC : show table definition
;;     q : quit and disconnect


(require 'edbi)


;;; [ company-edbi ]

(add-to-list 'company-backends 'company-edbi)



(require 'init-my-prog-lang-database-sqlite)
(require 'init-my-prog-lang-database-mongodb)
(require 'init-my-prog-lang-database-redis)


(provide 'init-my-prog-lang-database)

;;; init-my-prog-lang-database.el ends here
