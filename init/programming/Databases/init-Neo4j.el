;;; init-Neo4j.el --- init for Neo4j.

;;; Commentary:



;;; Code:

;;; [ n4js ] -- Neo4j shell in Emacs.

(use-package n4js
  :ensure t
  :defer t
  :commands (n4js-start)
  :init (defalias 'run-neo4j 'n4js-start)
  (setq n4js-cli-program "/usr/share/neo4j/bin/cypher-shell"))

;;; [ ob-neo4j ] -- Execute Neo4j's Cypher query in Org Mode Babel source block.

;;; FIXME PACKAGE-DESC error when installing
;; (use-package ob-neo4j
;;   :quelpa (ob-neo4j :fetcher github :repo "MarkBorcherding/ob-neo4j")
;;   :commands (org-babel-execute:neo4j)
;;   :config
;;   (add-to-list 'org-babel-load-languages '(neo4j . t))
;;   (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
;;   ;; (add-to-list 'org-babel-tangle-lang-exts '("neo4j" . "neo4j"))
;;   (add-to-list 'org-babel-default-header-args:cypher
;;                '(:eval . "yes"))
;;   (add-to-list 'org-babel-default-header-args:cypher
;;                '(:noweb . "yes"))
;;   (add-to-list 'org-babel-default-header-args:cypher
;;                '(:results . "output")))

;;; [ cypher-mode ] -- major mode for editing cypher scripts.

(use-package cypher-mode
  :ensure t
  :defer t)

;;; [ ob-cypher ] -- Query Neo4j using Cypher in Org Mode source blocks.

(use-package ob-cypher
  :ensure t
  :defer t
  :commands (org-babel-execute:cypher)
  :config
  (add-to-list 'org-babel-load-languages '(cypher . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("cypher" . "cypher"))
  (add-to-list 'org-babel-default-header-args:cypher
               '(:eval . "yes"))
  (add-to-list 'org-babel-default-header-args:cypher
               '(:noweb . "yes"))
  (add-to-list 'org-babel-default-header-args:cypher
               '(:results . "output")))



(provide 'init-Neo4j)

;;; init-Neo4j.el ends here
