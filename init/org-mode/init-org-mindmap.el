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
  :ensure t
  :defer t
  :custom ((org-roam-directory (expand-file-name "~/Org/org-roam"))
           (org-roam-mute-cache-build t))
  :hook (after-init . org-roam-mode)
  :preface
  (unless (boundp 'org-roam-prefix)
    (define-prefix-command 'org-roam-prefix))
  (global-set-key (kbd "C-c o m") 'org-roam-prefix)
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert)))
  :init
  ;; [ company-org-roam ] -- company backend for org-roam.
  (use-package company-org-roam
    :ensure t
    :preface
    (defun my/company-org-roam-setup ()
      (my-company-add-backend-locally 'company-org-roam))
    :hook (org-roam-mode . my/company-org-roam-setup))

  ;; [ org-roam-bibtex ] -- Connector between org-roam, BibTeX-completion, and org-ref.
  (use-package org-roam-bibtex
    :ensure t
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :bind (:map org-mode-map (("C-c n a" . orb-note-actions))))

  ;; [ org-roam-server ] -- A Web Application to Visualize the Org-Roam Database.
  (use-package org-roam-server
    :ensure t
    :commands (org-roam-server-mode)))



(provide 'init-org-mindmap)

;;; init-org-mindmap.el ends here
