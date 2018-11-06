;;; init-org-bibliography.el --- init for Org-mode bibliography.

;;; Commentary:



;;; Code:

;;; [ org-ref ] -- [C-c ] ] citations, cross-references, indexes, glossaries and bibtex utilities for Org-mode.

;; (use-package org-ref
;;   :ensure t
;;   :config
;;   ;; https://github.com/jkitchin/org-ref/issues/184
;;   (setq bibtex-completion-pdf-open-function 'org-open-file)
;;   ;; (setq org-ref-open-pdf-function )
;;   (setq org-latex-prefer-user-labels t)
;;   (setq org-ref-default-ref-type "ref") ; choose the default ref type for `org-ref'.
;;   (setq org-ref-show-broken-links nil) ; very poor performance.
;;   (setq org-ref-prefer-bracket-links t) ; wrap org-ref links with brackets.
;;   (setq org-ref-colorize-links t) ; colorize org-ref links.
;;
;;   ;; Let org-mode auto process the LaTeX export to PDF process.
;;   (add-to-list 'org-latex-pdf-process "bibtex %b" t)
;;
;;   ;; setup org-ref keybindings
;;   (setq org-ref-bibtex-hydra-key-binding (kbd "C-c ]"))
;;
;;   (define-key bibliograph-prefix (kbd "C-]") 'org-ref)
;;   (define-key bibliograph-prefix (kbd "C-l") 'org-ref-insert-link)
;;   (define-key bibliograph-prefix (kbd "r") 'org-ref-helm-insert-ref-link)
;;   (define-key bibliograph-prefix (kbd "c") 'org-ref-helm-insert-cite-link)
;;   (define-key bibliograph-prefix (kbd "l") 'org-ref-helm-insert-label-link)
;;
;;   ;; specify `org-ref' bibliography sources
;;   (require 'f)
;;   (setq org-ref-notes-directory "~/Org/Bibliography/"
;;         org-ref-bibliography-notes "~/Org/Bibliography/index.org"
;;         org-ref-default-bibliography (f-files "~/Org/Bibliography/"
;;                                               (lambda (f)
;;                                                 (f-ext? f "bib")))
;;         org-ref-bibliography-notes "~/Org/Bibliography/notes/"
;;         org-ref-pdf-directory "~/Org/Bibliography/lib/")
;;   )



(provide 'init-org-bibliography)

;;; init-org-bibliography.el ends here
