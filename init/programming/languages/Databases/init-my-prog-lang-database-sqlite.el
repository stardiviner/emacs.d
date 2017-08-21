;;; init-my-prog-lang-database-sqlite.el --- init SQLite

;;; Commentary:

;;; Code:

;;; [ sqlite.el ] -- use sqlite via elisp

;; (use-package sqlite
;;   :ensure t)


;;; [ ob-sqlite ]

(require 'ob-sqlite)

(add-to-list 'org-babel-load-languages '(sqlite . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("sqlite" . "sqlite"))

(setq org-babel-default-header-args:sqlite
      '((:db . "temp.db")
        (:results . "raw")
        ;; (:echo . t)
        (:column . t)
        (:nullvalue . "Null")))


;;; [ esqlite ] -- sqlite file manipulate utilities from Emacs.

;; (use-package esqlite
;;   :ensure t)


(provide 'init-my-prog-lang-database-sqlite)

;;; init-my-prog-lang-database-sqlite.el ends here
