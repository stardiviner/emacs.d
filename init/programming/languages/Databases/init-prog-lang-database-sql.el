;;; init-prog-lang-database-sql.el --- init for SQL
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ sql.el ] -- specialized comint.el for SQL interpreters.

(require 'sql)

(add-to-list 'display-buffer-alist
             '("^\\*SQL:.*\\*" (display-buffer-below-selected)))


;;; [ ob-sql ]

(require 'ob-sql)

(add-to-list 'org-babel-load-languages '(sql . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("sql" . "sql"))

(add-to-list 'org-src-lang-modes (cons "SQL" 'sql))

;; (add-to-list 'org-babel-default-header-args:sql
;;              '(:results . "table"))


;;; [ ob-sql-mode ] -- SQL code blocks evaluated by sql-mode.

(use-package ob-sql-mode
  ;; :ensure t
  :load-path "~/Code/Emacs/ob-sql-mode"
  :defer t
  :config
  (add-to-list 'org-babel-load-languages '(sql-mode . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  ;; (add-to-list 'org-babel-tangle-lang-exts '("sql-mode" . "sql"))

  ;; (add-to-list 'org-babel-default-header-args:sql-mode '(:product . "sqlite"))
  ;; (add-to-list 'org-babel-default-header-args:sql-mode '(:session . "ob-sql-mode"))

  ;; security guard
  (setq org-confirm-babel-evaluate
        (lambda (lang body) (string= lang "sql-mode")))
  )


;;; [ sql-indent]

(use-package sql-indent
  :ensure t
  :defer t
  :init
  (defun sql-indent-setup-local ()
    (make-local-variable 'indent-line-function)
    (setq-local indent-line-function 'sql-indent-line))
  (dolist (hook '(sql-mode-hook edbi:sql-mode-hook))
    (add-hook hook #'sql-indent-setup-local))
  )


;;; [ sqlup-mode ] -- An Emacs minor mode to upcase SQL keyword and functions.

(use-package sqlup-mode
  :ensure t
  :defer t
  :init
  (dolist (hook '(sql-mode-hook
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
  (define-key sql-mode-map (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)
  )


;;; [ sqled-mode ] -- major mode for editing sql, sqlplus, and pl/sql code.

;; (use-package sqled-mode
;;   :ensure t
;;   :defer t)


;;; [ edbi ]

(use-package edbi
  :ensure t
  :defer t
  :bind (:map sql-prefix
              ("d" . edbi:open-db-viewer))
  :config
  (setq edbi:completion-tool 'auto-complete) ; none

  (add-hook 'edbi:sql-mode-hook
            (lambda ()
              (define-key edbi:sql-mode-map (kbd "C-c C-q")
                'edbi:dbview-query-editor-quit-command)
              
              (sqlup-mode 1)

              ;; for `edbi:completion-tool'
              (add-hook 'completion-at-point-functions
                        'edbi:completion-at-point-function nil t)
              (company-mode 1)
              
              (auto-complete-mode -1)
              ))

  
  ;; [ edbi-minor-mode ] -- use edbi with regular SQL files.
  (use-package edbi-minor-mode
    :ensure t
    :init
    (add-hook 'sql-mode-hook
              (lambda ()
                (edbi-minor-mode)))
    )

  ;; [ edbi-sqlite ] -- edbi helper application
  (use-package edbi-sqlite
    :ensure t
    :bind (:map sql-prefix
                ("l" . edbi-sqlite))
    )

  ;; [ edbi-database-url ] -- run edbi with database url.
  (use-package edbi-database-url
    :ensure t
    :bind (:map sql-prefix
                ("u" . edbi-database-url))
    )

  ;; [ company-edbi ]
  (use-package company-edbi
    :ensure t
    :init
    (dolist (hook '(sql-mode-hook
                    sql-interactive-mode-hook
                    edbi:sql-mode-hook
                    ))
      (add-hook hook
                (lambda ()
                  (my-company-add-backend-locally 'company-edbi)
                  )))
    )
  )


;;; [ EmacSQL ] -- high-level SQL database front-end.

;; (use-package emacsql
;;   :ensure t
;;   :defer t)


;;; [ db-sql ] -- Connect to SQL server using tramp syntax.


;;; [ sql-complete ] -- support Oracle


;;; [ sql-completion ] -- support MySQL

;;; [ ejc-sql ] -- Emacs SQL client uses Clojure JDBC.

(use-package ejc-sql
  :ensure t
  :defer t
  ;; :config
  ;; ;; SQLite
  ;; (ejc-create-connection
  ;;  "SQLite-db-connection"
  ;;  :classpath (concat
  ;;              "~/.m2/repository/sqlitejdbc/sqlitejdbc/0.5.6/"
  ;;              "sqlitejdbc-0.5.6.jar")
  ;;  :classname ""
  ;;  :subprotocol "sqlite"
  ;;  :subname ""
  ;;  :user ""
  ;;  :password "")
  ;; ;; MySQL
  ;; (ejc-create-connection
  ;;  "MySQL-db-connection"
  ;;  :classpath (concat
  ;;              "~/.m2/repository/mysql/mysql-connector-java/5.1.6/"
  ;;              "mysql-connector-java-5.1.6.jar")
  ;;  :classname "com.mysql.jdbc.Driver"
  ;;  :subprotocol "mysql"
  ;;  :subname "//localhost:3306/my_db_name"
  ;;  :user "a_user"
  ;;  :password "secret")
  ;; ;; PostgreSQL
  ;; (ejc-create-connection
  ;;  "PostgreSQL-db-connection"
  ;;  :classpath (concat
  ;;              "~/.m2/repository/postgresql/postgresql/9.3-1102.jdbc41/"
  ;;              "postgresql-9.3-1102.jdbc41.jar")
  ;;  :classname ""
  ;;  :subprotocol "postgresql"
  ;;  :subname "//localhost:5432/test"
  ;;  :user "postgres"
  ;;  :password "324324")
  )


(provide 'init-prog-lang-database-sql)

;;; init-prog-lang-database-sql.el ends here
