;;; init-org-import.el --- Org Mode importer -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-10-15 08:35:53 stardiviner>

;;; Commentary:



;;; Code:

;;; [ org-pandoc-import ] -- Save yourself from non-org formats, thanks to pandoc.

(use-package org-pandoc-import
  :quelpa (org-pandoc-import :fetcher github :repo "tecosaur/org-pandoc-import" :files ("*.el" "filters" "preprocessors"))
  :commands (org-pandoc-import-transient-mode org-pandoc-import-to-org))



(provide 'init-org-import)

;;; init-org-import.el ends here
