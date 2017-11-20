;;; init-bibliography.el --- init for bibliography
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'bibliograph-prefix)
  (define-prefix-command 'bibliograph-prefix))
(define-key my-org-prefix (kbd "C-]") 'bibliograph-prefix)

;; (setq reftex-default-bibliography
;;       (quote
;;        ("default.bib" "other-default.bib")))


;;; [ bibtex ] -- BibTeX mode for GNU Emacs.

(use-package bibtex
  :defer t
  :config
  (use-package bibtex-style
    :ensure t)
  (use-package bibtex-utils
    :ensure t)
  )

;;; ----------------------------------------------------------------------------
;;; [ helm-bibtex ] -- Helm interface for bibliography manager.

(use-package helm-bibtex
  :ensure t
  :defer t
  :bind (:map bibliograph-prefix
              ("h" . helm-bibtex))
  :config
  (setq bibtex-completion-bibliography "~/Org/Bibliography/index.bib"
        bibtex-completion-library-path "~/Org/Bibliography/lib/"
        bibtex-completion-notes-path "~/Org/Bibliography/notes/"
        helm-bibtex-bibliography "~/Org/Bibliography/index.bib"
        helm-bibtex-library-path "~/Org/Bibliography/lib/")
  (setq bibtex-completion-pdf-open-function 'org-open-file ; `find-file'
        )
  )

;;; [ company-bibtex ] -- Emacs company-mode completion back-end for Bibtex keys.

(use-package company-bibtex
  :ensure t
  :defer t
  :init
  (setq company-bibtex-bibliography
        '("~/Org/Bibliography/index.bib"))

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



(provide 'init-bibliography)

;;; init-bibliography.el ends here
