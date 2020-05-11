;;; init-SQL.el --- init for SQL
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ sql.el ] -- specialized comint.el for SQL interpreters.

(require 'sql)

;;; add "postgres" as alias of "postgresql".
;;; Basically copy the `postgres' entry in `sql-product-alist' and add it as `postgresql'.
(let ((pg (cdr (assq 'postgres sql-product-alist))))  ; Make a copy of the postgres plist
  (add-to-list 'sql-product-alist                     ; Add as a new entry under postgresql
               (cons 'postgresql (plist-put pg :name "PostgreSQL"))))

;;; fix MySQL/MariaDB prompt incompatible.
(sql-set-product-feature 'mysql :prompt-regexp "^\\(MariaDB\\|MySQL\\) \\[[_a-zA-Z]*\\]> ")

(add-to-list 'display-buffer-alist
             '("^\\*SQL:.*\\*" (display-buffer-below-selected)))

(add-hook 'sql-mode-hook #'sql-indent-enable)
;;; enable () pairs auto insert in SQL buffer.
(add-hook 'sql-mode-hook #'electric-pair-local-mode)

;;; for command `sql-connect'.
(setq sql-connection-alist
      '((postgresql-test (sql-product 'postgres)
                         (sql-server "localhost")
                         (sql-port 5432)
                         (sql-user "postgres")
                         (sql-password "324324")
                         (sql-database "test"))
        (mysql-test (sql-product 'mysql)
                    (sql-server "localhost")
                    (sql-port 3306)
                    (sql-user "root")
                    (sql-password "324324")
                    (sql-database "test"))))

;;; [ sql-sqlline ] -- Adds SQLLine support to SQLi mode.

(use-package sql-sqlline
  :ensure t
  :commands (sql-sqlline))

;;; [ sql-indent] -- Support for indenting code in SQL files.

(use-package sql-indent
  :ensure t
  :commands (sqlind-minor-mode sqlind-show-syntax-of-line)
  :init (dolist (hook '(sql-mode-hook edbi:sql-mode-hook))
          (add-hook hook #'sqlind-minor-mode)))


;;; [ sqlup-mode ] -- An Emacs minor mode to upcase SQL keyword and functions.

(use-package sqlup-mode
  :ensure t
  :defer t
  :delight sqlup-mode
  :init (dolist (hook '(sql-mode-hook
                        sql-interactive-mode-hook
                        edbi:sql-mode-hook))
          (add-hook hook #'sqlup-mode))
  :config
  (defun my-sqlup-backward ()
    "Capitalization the word backward."
    (interactive)
    (backward-word) ; `backward-sexp'
    (upcase-word 1)
    (forward-char 1))

  (define-key sql-mode-map (kbd "C-c C-u") 'my-sqlup-backward)
  (define-key sql-mode-map (kbd "C-c u") 'sqlup-capitalize-keywords-in-region))

;;; [ sqlformat ] -- Reformat SQL using sqlformat or pgformatter.

;; (use-package sqlformat
;;   :ensure t
;;   :init (add-hook 'sql-mode-hook 'sqlformat-mode)
;;   (setq sqlformat-mode-format-on-save t))

;;; [ ob-sql ]

(use-package ob-sql
  :config
  (add-to-list 'org-babel-load-languages '(sql . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("sql" . "sql"))
  (add-to-list 'org-src-lang-modes (cons "SQL" 'sql))
  ;; (add-to-list 'org-babel-default-header-args:sql
  ;;              '(:results . "table"))
  )

;;; [ ob-sql-mode ] -- SQL code blocks evaluated by sql-mode.

;; (use-package ob-sql-mode
;;   ;; :ensure t
;;   :load-path "~/Code/Emacs/ob-sql-mode"
;;   :defer t
;;   :commands (org-babel-execute:sql-mode)
;;   :config
;;   (add-to-list 'org-babel-load-languages '(sql-mode . t))
;;   (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
;;   ;; (add-to-list 'org-babel-tangle-lang-exts '("sql-mode" . "sql"))
;;
;;   ;; (add-to-list 'org-babel-default-header-args:sql-mode '(:product . "sqlite"))
;;   ;; (add-to-list 'org-babel-default-header-args:sql-mode '(:session . "ob-sql-mode"))
;;
;;   ;; security guard
;;   (setq org-confirm-babel-evaluate
;;         (lambda (lang body) (string= lang "sql-mode"))))

;;; [ ejc-sql ] -- Emacs SQL client uses Clojure JDBC.

(use-package ejc-sql
  :ensure t
  :defer t
  :commands (ejc-connect
             ejc-connect-existing-repl ejc-connect-interactive ejc-sql-mode
             ejc-get-temp-editor-buffer)
  :config
  (setq nrepl-sync-request-timeout 60
        ejc-connection-validate-timeout 60)
  
  (with-eval-after-load 'ob-async
    (add-to-list 'ob-async-no-async-languages-alist "sql"))

  (defun my/ejc-sql-settings ()
    (setq-local ac-auto-start 2)
    (setq-local ac-delay 0)
    (setq-local ac-auto-show-menu t)
    (setq-local ac-use-quick-help nil)
    (company-mode -1)
    (flyspell-mode -1))
  (add-hook 'ejc-sql-mode-hook #'my/ejc-sql-settings)

  (add-hook 'ejc-sql-mode-hook #'ejc-eldoc-setup)

  (add-to-list 'display-buffer-alist
               '("^\\*ejc-sql-output\\*" (display-buffer-below-selected)))
  
  (defun my-ejc-sql-ac-setup ()
    (ejc-sql-mode 1)
    (unless (eq major-mode 'org-mode)
      (auto-complete-mode 1))
    (ejc-ac-setup))
  (add-hook 'sql-mode-hook #'my-ejc-sql-ac-setup)

  ;; enable `ejc-sql' in `sql-interactive-mode'.
  (defun ejc-sql-interactive-mode-setup ()
    "Setup ejc-sql completion in `sql-interactive-mode'."
    (interactive)
    (call-interactively 'ejc-connect)
    (ejc-sql-mode)
    (auto-complete-mode 1)
    (ejc-ac-setup))
  (add-hook 'sql-interactive-mode #'ejc-sql-interactive-mode-setup)
  
  (ejc-create-connection
   "PostgreSQL-db-postgres"
   :dependencies [[org.postgresql/postgresql "42.2.5.jre7"]]
   ;; :classpath (concat "~/.m2/repository/org/postgresql/postgresql/"
   ;;                    "42.2.5.jre7/postgresql-42.2.5.jre7.jar")
   :dbtype "postgresql"
   :host "localhost"
   :port "5432"
   :user "postgres"
   :password (my/json-read-value my/account-file 'ejc-sql-postgresql)
   :dbname "test")

  (ejc-create-connection
   "MySQL-db-test"
   :dependencies [[mysql/mysql-connector-java "5.1.32"]]
   ;; :classpath (concat "~/.m2/repository/mysql/mysql-connector-java/"
   ;;                    "5.1.32/mysql-connector-java-5.1.32.jar")
   :dbtype "mysql"
   :host "localhost"
   :port "3306"
   :user "root"
   :password (my/json-read-value my/account-file 'ejc-sql-mysql)
   :dbname "test")

  (ejc-create-connection
   "SQLite-db-temp"
   :dependencies [[org.xerial/sqlite-jdbc "3.25.2"]]
   ;; :classpath (concat "~/.m2/repository/org/xerial/sqlite-jdbc/"
   ;;                    "3.25.2/sqlite-jdbc-3.25.2.jar")
   :subprotocol "sqlite"
   :subname (file-truename "~/test.db"))

  (defun ejc-connect-sqlite (sqlite-db-file)
    "A helper command to dynamically create SQLite Driver connection."
    (interactive "fSQLite db file: ")
    (let ((connection-name (read-string "Input SQLite connection name: ")))
      (ejc-create-connection
       connection-name
       :classpath (concat "~/.m2/repository/org/xerial/sqlite-jdbc/"
                          "3.25.2/sqlite-jdbc-3.25.2.jar")
       :subprotocol "sqlite"
       :subname (file-truename sqlite-db-file))
      (ejc-connect connection-name)))

  ;; press [q] to close ejc-sql result window.
  (add-hook 'ejc-result-mode-hook (lambda () (local-set-key (kbd "q") 'delete-window))))

;;; [ icsql ] -- This library provides an Emacs SQL mode integration to the ciSQL program.

;; (use-package icsql
;;   :ensure t
;;   :commands (icsql)
;;   :init (setq icsql-connections '()))

;;; [ edbi ]

;; (use-package edbi
;;   :ensure t
;;   :defer t
;;   :commands (edbi:open-db-viewer)
;;   :config
;;   (setq edbi:completion-tool 'auto-complete) ; none
;;
;;   (add-hook 'edbi:sql-mode-hook
;;             (lambda ()
;;               (define-key edbi:sql-mode-map (kbd "C-c C-q")
;;                 'edbi:dbview-query-editor-quit-command)
;;               (sqlup-mode 1)
;;               ;; for `edbi:completion-tool'
;;               (add-hook 'completion-at-point-functions
;;                         'edbi:completion-at-point-function nil t)
;;               (company-mode 1)
;;               (auto-complete-mode -1)
;;               ))
;;
;;   ;; [ edbi-minor-mode ] -- use edbi with regular SQL files.
;;   (use-package edbi-minor-mode
;;     :ensure t
;;     :init (add-hook 'sql-mode-hook (lambda () (edbi-minor-mode))))
;;
;;   ;; [ edbi-sqlite ] -- edbi helper application
;;   (use-package edbi-sqlite
;;     :ensure t
;;     :commands (edbi-sqlite))
;;
;;   ;; [ edbi-database-url ] -- run edbi with database url.
;;   (use-package edbi-database-url
;;     :ensure t
;;     :commands (edbi-database-url))
;;
;;   ;; [ company-edbi ]
;;   (use-package company-edbi
;;     :ensure t
;;     :init
;;     (defun my/company-edbi-setup ()
;;       (my-company-add-backend-locally 'company-edbi))
;;     (dolist (hook '(sql-mode-hook
;;                     sql-interactive-mode-hook
;;                     edbi:sql-mode-hook))
;;       (add-hook hook #'my/company-edbi-setup)))
;;   )

;;; [ EmacSQL ] -- high-level SQL database front-end.

;; (use-package emacsql
;;   :ensure t
;;   :defer t)

;;; [ db-sql ] -- Connect to SQL server using tramp syntax.


;;; [ format-table ] -- Parse and reformat tabular data in Emacs.

(use-package format-table
  :ensure t
  :commands (format-table))

;;; [ emacs-db ] -- very simple database for emacslisp, can also wrap other databases.

;; (use-package db
;;   :ensure t
;;   :defer t)

(require 'init-SQLite)
(require 'init-MySQL)
(require 'init-PostgreSQL)


(provide 'init-SQL)

;;; init-SQL.el ends here
