;;; init-my-prog-lang-database-sql.el --- init for SQL
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ sql.el ] -- specialized comint.el for SQL interpreters.

;;; Usage:
;;
;; - `sql-mode' / `sql-interactive-mode' in SQLi buffer "*SQL*"
;; - [M-x sql-help] ::
;; - [M-x sql-set-sqli-buffer RET *SQL* RET] :: To interact with the interpreter from a window already in SQL mode.

(require 'sql)

;; If you don’t like window splittings related to the SQL buffer, try the following, per Force Same Window.
;; (add-to-list 'same-window-buffer-names "*SQL*")


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

(autoload 'sqlup-mode "sqlup-mode")

(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-inretactive-mode-hook 'sqlup-mode)

;; (add-hook 'sqlup-mode-hook
;;           (lambda ()
;;             (setq sqlup-keywords
;;                   (append sqlup-keywords
;;                           '("text" "glob" "offset")))))


;;; [ sqled-mode ] -- major mode for editing sql, sqlplus, and pl/sql code.

(require 'sqled-mode)


;;; [ sqlplus ] -- user friendly interface to SQL*Plus and support for PL/SQL compilation.

;;; Usage:
;;
;; - [M-x sqlplus] :: start new SQL*Plus session.
;;
;;   - [C-RET]   :: execute command under point.
;;   - [S-C-RET] :: execute command under point, and show result table in HTML buffer.
;;   - [M-RET]   :: explain execution plan for command under point.
;;   - [M-.] / [C-mouse-1] :: find database object definition (table, view index, synonym, trigger, procedure,
;;                            function, package) in filesystem.
;;   - [C-c C-s] :: show database object definition (retrieved from database).
;;
;; TODO: customize sqlplus group.

;; (require 'sqlplus)
;; (add-to-list 'auto-mode-alist '("\\.sqp\\'" . sqlplus-mode))

;;  If you want PL/SQL support also, try something like this:
;;
;;  (require 'plsql)
;;  (setq auto-mode-alist
;;    (append '(("\\.pls\\'" . plsql-mode) ("\\.pkg\\'" . plsql-mode)
;; 		("\\.pks\\'" . plsql-mode) ("\\.pkb\\'" . plsql-mode)
;; 		("\\.sql\\'" . plsql-mode) ("\\.PLS\\'" . plsql-mode) 
;; 		("\\.PKG\\'" . plsql-mode) ("\\.PKS\\'" . plsql-mode)
;; 		("\\.PKB\\'" . plsql-mode) ("\\.SQL\\'" . plsql-mode)
;; 		("\\.prc\\'" . plsql-mode) ("\\.fnc\\'" . plsql-mode)
;; 		("\\.trg\\'" . plsql-mode) ("\\.vw\\'" . plsql-mode)
;; 		("\\.PRC\\'" . plsql-mode) ("\\.FNC\\'" . plsql-mode)
;; 		("\\.TRG\\'" . plsql-mode) ("\\.VW\\'" . plsql-mode))
;; 	      auto-mode-alist ))




;;; [ edbi ]

;;; Usage:
;;
;; - M-x `edbi:open-db-viewer' opens a dialog for DB connection.
;;
;;     - Data Source : URI string for DBI::connect (Ex. dbi:SQLite:dbname=/path/db.sqlite )
;;     - User Name, Auth : user name and password for DBI::connect
;;     - History button : you can choose a data source from your connection history.
;;     - OK button : connect DB and open the database view
;;
;; Check the key-bind `edbi:dbview-keymap'.
;;
;;     j,k, n,p : navigation for rows
;;     c : switch to query editor buffer
;;     RET : show table data
;;     SPC : show table definition
;;     q : quit and disconnect

;; install Perl packages:
;; $ [cpanp -i / cpan] RPC::EPC::Service DBI DBD::SQLite DBD::Pg DBD::mysql

(require 'edbi)

(define-key my-prog-database-map (kbd "d") 'edbi:open-db-viewer)


;;; [ edbi-minor-mode ] -- use edbi with regular SQL files.

;;; Usage:
;;
;; -

(require 'edbi-minor-mode)



;;; [ edbi-sqlite ] -- edbi helper application

;;; Usage:
;;
;; 1. $ sqlite3
;; 2. `edbi-sqlite'

(require 'edbi-sqlite)


;;; [ edbi-database-url ] -- run edbi with database url.

;;; Usage:
;;
;; Specify database url with environment variable
;;
;;   M-x setenv RET DATABASE_URL RET pgsql://me:secret@localhost:5678/test
;;
;; Connect to you database
;;
;;   M-x edbi-database-url
;;
;; Optionally you can specify database url by marking region or type it
;; interactively.

(require 'edbi-database-url)


;;; [ company-edbi ]

(require 'company-edbi)

(dolist (hook '(sql-mode-hook
                sql-interactive-mode-hook
                edbi:sql-mode-hook
                ))
  (add-hook hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           'company-edbi))))


;;; [ db-sql ] -- Connect to SQL server using tramp syntax.

(require 'db-sql)


;;; [ sql-complete ] -- support Oracle


;;; [ sql-completion ] -- support MySQL


(provide 'init-my-prog-lang-database-sql)

;;; init-my-prog-lang-database-sql.el ends here
