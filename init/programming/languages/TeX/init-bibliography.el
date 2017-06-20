;;; init-bibliography.el --- init for bibliography
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------

;; (setq reftex-default-bibliography
;;       (quote
;;        ("default.bib" "other-default.bib")))


;;; [ bibtex ] -- BibTeX mode for GNU Emacs.

(use-package bibtex
  :config
  ;; (setq bibtex-parse-keys-fast t
  ;;       bibtex-parse-keys-timeout 60
  ;;       )
  (use-package bibtex-style
    :ensure t)
  (use-package bibtex-utils
    :ensure t)
  )

;;; ----------------------------------------------------------------------------
;;; [ helm-bibtex ] -- Helm interface for bibliography manager.

(use-package helm-bibtex
  :ensure t
  :config
  (setq bibtex-completion-bibliography "~/Org/Bibliography/bibliography.bib"
        bibtex-completion-library-path "~/Org/Bibliography/PDFs/"
        bibtex-completion-notes-path "~/Org/Bibliography/notes/")
  (setq bibtex-completion-pdf-open-function 'org-open-file ; `find-file'
        )
  )

;;; [ company-bibtex ] -- Emacs company-mode completion back-end for Bibtex keys.

(use-package company-bibtex
  :ensure t
  :config
  (setq company-bibtex-bibliography
        '("~/Org/Bibliography/bibliography.bib"
          ))

  (add-hook 'org-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'company-bibtex)))
  )

;;; [ ebib ] -- a BibTeX database manager.

;; (use-package ebib
;;   :ensure t
;;   :config
;;   (use-package ebib-handy
;;     :ensure t)
;;   )

;;; [ bibretrieve ] -- Retrieving BibTeX entries from the web.

;; (use-package bibretrieve
;;   :ensure t)


;;; [ org-ref ] -- citations, cross-references, indexes, glossaries and bibtex utilities for Org-mode.

(use-package org-ref
  :ensure t
  :config
  ;; https://github.com/jkitchin/org-ref/issues/184
  (setq bibtex-completion-pdf-open-function 'org-open-file)
  ;; (setq org-ref-open-pdf-function )
  (setq org-latex-prefer-user-labels t)
  (setq org-ref-show-broken-links nil) ; very poor performance.
  (setq org-ref-prefer-bracket-links t) ; wrap org-ref links with brackets.

  ;; Let org-mode auto process the LaTeX export to PDF process.
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))

  ;; setup org-ref keybindings
  (setq org-ref-bibtex-hydra-key-binding (kbd "C-c ]"))

  (unless (boundp 'org-ref-prefix)
    (define-prefix-command 'org-ref-prefix))
  (define-key my-org-prefix (kbd "C-]") 'org-ref-prefix)

  (define-key org-ref-prefix (kbd "C-]") 'org-ref-insert-link)
  (define-key org-ref-prefix (kbd "c") 'org-ref-helm-insert-cite-link)
  (define-key org-ref-prefix (kbd "l") 'org-ref-helm-insert-label-link)
  (define-key org-ref-prefix (kbd "r") 'org-ref-helm-insert-ref-link)

  ;; specify `org-ref' bibliography sources
  (require 'f)
  (setq org-ref-default-bibliography (f-files "~/Org/Bibliography/"
                                              (lambda (f)
                                                (f-ext? f "bib")))
        org-ref-bibliography-notes "~/Org/Bibliography/notes/"
        org-ref-pdf-directory "~/Org/Bibliography/PDFs/")
  )

;;; ----------------------------------------------------------------------------

(provide 'init-bibliography)

;;; init-bibliography.el ends here
