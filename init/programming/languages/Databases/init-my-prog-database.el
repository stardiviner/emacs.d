;;; init-my-prog-database.el --- init Databases settings

;;; Commentary:



;;; Code:

;;; [ sql-indent]
;; (require 'sql-indent)



;;; Auto Uppercase SQL Keywords
(setq sql-abbrev-file-name "~/.emacs.d/init/abbrevs/sql.abbrev")
(if (file-exists-p sql-abbrev-file-name)
    (load sql-abbrev-file-name))


(provide 'init-my-prog-database)

;;; init-my-prog-database.el ends here
