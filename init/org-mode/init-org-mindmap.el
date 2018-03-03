;;; init-org-mindmap.el --- Org-mode for Mind Mapping

;;; Commentary:



;;; Code:
;;; [ Mindmap (`org-freemind') ] -- Creates a directed graph from org-mode files.

(require 'ox-freemind)

;;; [ org-mind-map ] -- creates graphviz directed mind-map graphs.

(use-package org-mind-map
  :ensure t
  :commands (org-mind-map-write)
  )

;;; [ org-brain ] -- Org-mode wiki + concept-mapping.

;; (use-package org-brain :ensure t
;;   :init
;;   (setq org-brain-path "directory/path/where-i-want-org-brain")
;;   ;; For Evil users
;;   (if (featurep 'evil)
;;       (eval-after-load 'evil
;;         (evil-set-initial-state 'org-brain-visualize-mode 'emacs)))
;;   :config
;;   (setq org-id-track-globally t)
;;   (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
;;   (push '("b" "Brain" plain (function org-brain-goto-end)
;;           "* %i%?" :empty-lines 1)
;;         org-capture-templates)
;;   (setq org-brain-visualize-default-choices 'all)
;;   (setq org-brain-title-max-length 12)
;;   )



(provide 'init-org-mindmap)

;;; init-org-mindmap.el ends here
