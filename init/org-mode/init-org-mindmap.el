;;; init-org-mindmap.el --- Org-mode for Mind Mapping

;;; Commentary:



;;; Code:
;;; [ Mindmap (`org-freemind') ] -- Creates a directed graph from org-mode files.

(require 'ox-freemind)

;;; [ org-mind-map ] -- creates graphviz directed mind-map graphs.

(use-package org-mind-map
  :ensure t
  :commands (org-mind-map-write))



(provide 'init-org-mindmap)

;;; init-org-mindmap.el ends here
