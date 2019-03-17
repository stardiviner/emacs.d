;;; init-elasticsearch.el --- init for ElasticSearch.

;;; Commentary:



;;; Code:

;;; [ es-mode ] -- An Emacs major mode for editing Elasticsearch requests.

(use-package es-mode
  :ensure t
  :ensure-system-package jq
  :defer t
  :mode ("\\.es$" . es-mode)
  :init
  (use-package ob-elasticsearch
    :defer t
    :commands (org-babel-execute:elasticsearch)
    :config
    (add-to-list 'org-babel-load-languages '(elasticsearch . t))
    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
    (add-to-list 'org-babel-tangle-lang-exts '("elasticsearch" . "es"))
    ;; (add-to-list 'org-babel-default-header-args:es (:jq . nil))
    ;; (add-to-list 'org-babel-default-header-args:es '(:tablify . nil))
    )
  :config
  (setq es-always-pretty-print t)

  ;; yasnippet support.
  (es-mode-snippets-initialize)
  
  ;; ES Results Buffers
  (add-to-list 'display-buffer-alist
               '("^\\*ES:.*\\*" . (display-buffer-below-selected)))
  ;; using hide-show mode in results buffers.
  (add-hook 'es-result-mode-hook 'hs-minor-mode))



(provide 'init-elasticsearch)

;;; init-elasticsearch.el ends here
