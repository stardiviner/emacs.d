;;; init-org-mindmap.el --- Org-mode for Mind Mapping

;;; Commentary:



;;; Code:
;;; [ Mindmap (`org-freemind') ] -- Creates a directed graph from org-mode files.

(require 'ox-freemind)

;;; [ org-mind-map ] -- creates graphviz directed mind-map graphs.

(use-package org-mind-map
  :ensure t
  :commands (org-mind-map-write))

;;; [ org-roam ] -- Rudimentary Roam replica with Org Mode.

(use-package org-roam
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory org-directory)
  :preface
  (unless (boundp 'org-roam-prefix)
    (define-prefix-command 'org-roam-prefix))
  (global-set-key (kbd "C-c o m") 'org-roam-prefix)
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))



(provide 'init-org-mindmap)

;;; init-org-mindmap.el ends here
