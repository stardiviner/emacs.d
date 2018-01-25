;;; init-elasticsearch.el --- init for ElasticSearch.

;;; Commentary:



;;; Code:

;;; [ es-mode ] -- An Emacs major mode for editing Elasticsearch requests.

(use-package es-mode
  :ensure t
  :ensure-system-package jq
  :mode ("\\.es$" . es-mode)
  :config
  ;; [ ob-elasticsearch (ob-es) ]
  (require 'ob-elasticsearch)
  (add-to-list 'org-babel-load-languages '(elasticsearch . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("elasticsearch" . "es"))
  ;; (add-to-list 'org-babel-default-header-args:es (:jq . nil))
  ;; (add-to-list 'org-babel-default-header-args:es '(:tablify . nil))
  )



(provide 'init-elasticsearch)

;;; init-elasticsearch.el ends here
