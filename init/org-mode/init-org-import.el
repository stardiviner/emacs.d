;;; init-org-import.el --- Org Mode importer -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-10-15 08:35:53 stardiviner>

;;; Commentary:



;;; Code:

;;; [ org-pandoc-import ] -- Save yourself from non-org formats, thanks to pandoc.

(use-package org-pandoc-import
  ;; :quelpa (org-pandoc-import :fetcher github :repo "tecosaur/org-pandoc-import" :files ("*.el" "filters" "preprocessors"))
  :load-path "~/Code/Emacs/org-pandoc-import"
  :commands (org-pandoc-import-as-org org-pandoc-import-to-org)
  :config
  (require 'org-pandoc-import-transient)
  (org-pandoc-import-transient-mode 1))

;;; [ xmind-org ] -- Generate Org mode outlines from an XMind mindmap file.

(use-package xmind-org
  :ensure t
  :commands (xmind-org-insert-file))



(provide 'init-org-import)

;;; init-org-import.el ends here
