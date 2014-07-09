;;; init-my-prog-database.el --- init Databases settings

;;; Commentary:



;;; Code:

;;; [ sql-indent]
;; (require 'sql-indent)



;;; Auto Uppercase SQL Keywords
(unless (not (featurep 'sqlup-mode))
  (setq sql-abbrev-file-name "~/.emacs.d/init/abbrevs/sql.abbrev")
  (if (file-exists-p sql-abbrev-file-name)
      (load sql-abbrev-file-name)))

;;; sqlup-mode
;;; Usage: The capitalization is triggered when you press 'SPC', ';' or '('.
(if (featurep 'sqlup-mode)
    (add-hook 'sql-mode-hook 'sqlup-mode))


(provide 'init-my-prog-database)

;;; init-my-prog-database.el ends here
