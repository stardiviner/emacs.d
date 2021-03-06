;;; init-bibliography.el --- init for bibliography
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'bibliograph-prefix)
  (define-prefix-command 'bibliograph-prefix))
(define-key Org-prefix (kbd "C-]") 'bibliograph-prefix)

;; (setq reftex-default-bibliography
;;       (quote
;;        ("default.bib" "other-default.bib")))


;;; [ bibtex ] -- BibTeX mode for GNU Emacs.

(use-package bibtex)
(use-package bibtex-style)

(use-package bibtex-utils
  :ensure t)

;;; ----------------------------------------------------------------------------
;;; [ bibtex-completion ] -- A BibTeX backend for completion frameworks.

(use-package bibtex-completion
  :ensure t
  :defer t)

;;; [ helm-bibtex ] -- Helm interface for bibliography manager.

(use-package helm-bibtex
  :if (featurep 'helm)
  :ensure t
  :defer t
  :commands (helm-bibtex helm-bibtex-with-local-bibliography)
  :bind (:map bibliograph-prefix ("h" . helm-bibtex))
  :config
  (setq bibtex-completion-bibliography "~/Org/Bibliography/index.bib"
        bibtex-completion-library-path "~/Org/Bibliography/lib/"
        bibtex-completion-notes-path "~/Org/Bibliography/notes/"
        helm-bibtex-bibliography "~/Org/Bibliography/index.bib"
        helm-bibtex-library-path "~/Org/Bibliography/lib/")
  (setq bibtex-completion-pdf-open-function 'org-open-file))

;;; [ company-bibtex ] -- Emacs company-mode completion back-end for Bibtex keys.

(use-package company-bibtex
  :ensure t
  :defer t
  :init (setq company-bibtex-bibliography '("~/Org/Bibliography/index.bib"))
  (defun my/company-bibtex-setup ()
    (my-company-add-backend-locally 'company-bibtex))
  (add-hook 'org-mode-hook 'my/company-bibtex-setup))

;;; [ ebib ] -- a BibTeX database manager.

;; (use-package ebib
;;   :ensure t
;;   :ensure ebib-handy)

;;; [ bibretrieve ] -- Retrieving BibTeX entries from the web.

;; (use-package bibretrieve
;;   :ensure t)

;; (use-package ivy-bibtex
;;   :if (featurep 'ivy)
;;   :ensure t
;;   :defer t
;;   :commands (ivy-bibtex ivy-bibtex-with-local-bibliography)
;;   :init (setq bibtex-completion-pdf-symbol "⌘"
;;               bibtex-completion-notes-symbol "✎"
;;               bibtex-completion-additional-search-fields '(keywords)))


(provide 'init-bibliography)

;;; init-bibliography.el ends here
