;;; init-SQL.el --- init for SQL
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ sql.el ] -- specialized comint.el for SQL interpreters.

(require 'sql)

;;; fix MySQL/MariaDB prompt incompatible.
(sql-set-product-feature 'mysql :prompt-regexp "^\\(MariaDB\\|MySQL\\) \\[[_a-zA-Z]*\\]> ")

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
  :config
  (defun sql-indent-setup-local ()
    (make-local-variable 'indent-line-function)
    (setq-local indent-line-function 'sql-indent-line))
  (dolist (hook '(sql-mode-hook edbi:sql-mode-hook))
    (add-hook hook #'sql-indent-setup-local)))


;;; [ sqlup-mode ] -- An Emacs minor mode to upcase SQL keyword and functions.

(use-package sqlup-mode
  :ensure t
  :defer t
  :delight sqlup-mode
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

;;; [ sqlformat ] -- Reformat SQL using sqlformat or pgformatter.

(use-package sqlformat
  :ensure t
  :init (add-hook 'sql-mode-hook 'sqlformat-mode)
  ;; (setq sqlformat-mode-format-on-save t)
  )

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
;;     (dolist (hook '(sql-mode-hook
;;                     sql-interactive-mode-hook
;;                     edbi:sql-mode-hook))
;;       (add-hook hook (lambda () (my-company-add-backend-locally 'company-edbi)))))
;;   )

;;; [ EmacSQL ] -- high-level SQL database front-end.

;; (use-package emacsql
;;   :ensure t
;;   :defer t)

;;; [ db-sql ] -- Connect to SQL server using tramp syntax.

;;; [ ejc-sql ] -- Emacs SQL client uses Clojure JDBC.

(use-package ejc-sql
  ;; :ensure t
  :load-path "~/Code/Emacs/ejc-sql/"
  :load (ejc-interaction)
  :commands (ejc-connect ejc-connect-existing-repl ejc-sql-mode)
  :init (setq nrepl-sync-request-timeout 60
              ejc-connection-validate-timeout 60)
  :config
  (add-to-list 'ob-async-no-async-languages-alist "sql")

  (add-hook 'ejc-sql-mode-hook
            (lambda ()
              (setq-local ac-delay 0.2)
              (company-mode -1)))

  (add-to-list 'display-buffer-alist
               '("^\\*ejc-sql-output\\*" (display-buffer-below-selected)))
  
  (defun my-ejc-sql-ac-setup ()
    (ejc-sql-mode 1)
    (unless (eq major-mode 'org-mode)
      (auto-complete-mode 1))
    (ejc-ac-setup))
  (add-hook 'sql-mode-hook #'my-ejc-sql-ac-setup)

  ;; (advice-add 'ejc-connect :before
  ;;             #'(lambda (connection-name)
  ;;                 (switch-to-buffer-other-window (format "*ejc-sql %s*" connection-name))))
  
  (ejc-create-connection
   "PostgreSQL-db-postgres"
   :classpath (concat "~/.m2/repository/postgresql/postgresql/"
                      "9.3-1102.jdbc41/postgresql-9.3-1102.jdbc41.jar")
   :dbtype "postgresql"
   :host "localhost"
   :port "5432"
   :user "postgres"
   :password "****"
   :dbname "test")

  (ejc-create-connection
   "MySQL-db-test"
   :classpath (concat "~/.m2/repository/mysql/mysql-connector-java/"
                      "5.1.32/mysql-connector-java-5.1.32.jar")
   :dbtype "mysql"
   :host "localhost"
   :port "3306"
   :user "root"
   :password "****"
   :dbname "test")

  (ejc-create-connection
   "SQLite-db-temp"
   :classpath (concat "~/.m2/repository/org/xerial/sqlite-jdbc/"
                      "3.25.2/sqlite-jdbc-3.25.2.jar")
   :subprotocol "sqlite"
   :subname (file-truename "~/test.db"))
  )


(provide 'init-SQL)

;;; init-SQL.el ends here
