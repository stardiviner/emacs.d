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

(eval-after-load "sql"
  '(load-library "sql-indent"))

(dolist (hook '(sql-mode-hook
                sql-interactive-mode-hook
                edbi:sql-mode-hook
                ))
  (add-hook hook
            (function (lambda ()
                        (make-local-variable 'indent-line-function)
                        (setq indent-line-function 'sql-indent-line)))))


;;; Auto Uppercase SQL Keywords
;; (unless (not (featurep 'sqlup-mode))
;;   (setq sql-abbrev-file-name "~/.emacs.d/init/abbrevs/sql.abbrev")
;;   (if (file-exists-p sql-abbrev-file-name)
;;       (load sql-abbrev-file-name)))


;;; [ sqlup-mode ]

;;; Usage:
;; - The capitalization is triggered when you press 'SPC', ';', ',', or '(', '\r' (Enter),
;; - [C-c u] region :: `sqlup-capitalize-keywords-in-region'

(use-package sqlup-mode
  :init
  (define-key sql-mode-map (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)
  :config
  (defun my-sqlup-backward ()
    "Capitalization the word backward."
    (interactive)
    (backward-word) ; `backward-sexp'
    (upcase-word 1)
    (forward-char 1)
    )

  (define-key sql-mode-map (kbd "C-c C-u") 'my-sqlup-backward)

  (dolist (hook '(sql-mode-hook
                  sql-interactive-mode-hook
                  ))
    (add-hook hook
              '(lambda ()
                 (sqlup-mode 1))))

  ;; TODO:
  ;; (add-hook 'sqlup-mode-hook
  ;;           (lambda ()
  ;;             (setq sqlup-keywords
  ;;                   (append sqlup-keywords
  ;;                           '("text" "glob" "offset")))))
  )


;;; [ sqled-mode ] -- major mode for editing sql, sqlplus, and pl/sql code.

;; (require 'sqled-mode)


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

;;; keymaps:
;;
;; - `edbi:dbview-keymap'
;; - `edbi:dbview-table-keymap'
;; - `ctbl:table-mode-map'
;; - `edbi:sql-mode-map'
;; - `edbi:dbview-query-result-keymap'

(define-key my-inferior-db-sql-map (kbd "d") 'edbi:open-db-viewer)

(add-hook 'edbi:sql-mode-hook
          '(lambda ()
             (define-key edbi:sql-mode-map (kbd "C-c C-q") 'edbi:dbview-query-editor-quit-command)
             
             (sqlup-mode 1)
             ))


;;; [ edbi-minor-mode ] -- use edbi with regular SQL files.

(add-hook 'sql-mode-hook
          (lambda ()
            (edbi-minor-mode)))


;;; [ edbi-sqlite ] -- edbi helper application

;;; Usage:
;;
;; `edbi-sqlite'

;; (defun my-edbi-sqlite ()
;;   (interactive)
;;   (if (not (process-live-p (get-process "sqlite3 process")))
;;       (start-process-shell-command "sqlite3 process" "*SQLite process*" "sqlite3")
;;     (edbi-sqlite)                       ; FIXME: interactive command.
;;     )
;;   )

(define-key my-inferior-db-sql-map (kbd "l") 'edbi-sqlite)


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

(define-key my-inferior-db-sql-map (kbd "u") 'edbi-database-url)


;;; [ company-edbi ]

(dolist (hook '(sql-mode-hook
                sql-interactive-mode-hook
                edbi:sql-mode-hook
                ))
  (add-hook hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           'company-edbi))))


;;; [ EmacSQL ] -- high-level SQL database front-end.


;;; [ db-sql ] -- Connect to SQL server using tramp syntax.


;;; [ sql-complete ] -- support Oracle


;;; [ sql-completion ] -- support MySQL


(provide 'init-my-prog-lang-database-sql)

;;; init-my-prog-lang-database-sql.el ends here
