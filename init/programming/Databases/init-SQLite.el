;;; init-SQLite.el --- init SQLite

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
      '((:column . t)
        ;; (:line . "yes")
        (:header . "yes")
        ;; (:echo . "yes")
        ;; (:bail . "yes")
        ;; (:separator . ",")
        ;; (:csv . t)
        ;; (:html . t)
        (:results . "verbatim")
        ;; (:wrap . "example")
        (:nullvalue . "Null")
        (:db . "data/code/temp.db") ; this is essential necessary for every src block.
        ))


;;; [ esqlite ] -- sqlite file manipulate utilities from Emacs.

;; (use-package esqlite
;;   :ensure t)


(provide 'init-SQLite)

;;; init-SQLite.el ends here
