;;; init-bibliography.el --- init for bibliography
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ bibtex ] -- BibTeX mode for GNU Emacs.

(use-package bibtex
  :ensure t
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
  :defer t)

;;; [ ivy-bibtex ] -- A BibTeX bibliography manager based on Ivy.

(use-package ivy-bibtex
  :ensure t
  :defer t)

;;; [ company-bibtex ] -- Emacs company-mode completion back-end for Bibtex keys.

(use-package company-bibtex
  :ensure t
  :config
  (setq company-bibtex-bibliography
        '("~/.emacs.d/bibliography/bibliography.bib"
          ))

  (add-to-list 'company-backends 'company-bibtex t)
  )

;;; ----------------------------------------------------------------------------

(provide 'init-bibliography)

;;; init-bibliography.el ends here
