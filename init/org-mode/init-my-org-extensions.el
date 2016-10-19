;;; init-my-org-extensions.el --- init for Org Extensions
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ org-plot ] -- Support for plotting from Org-mode.

(require 'org-plot)


;;; [ org-bbdb ] -- Support for links to BBDB entries from within Org-mode.

;; - [C-c C-l] + `bbdb:' link.

;; (require 'org-bbdb)
;;
;; ;; 'anniversary, 'birthday,
;; (setq org-bbdb-anniversary-field 'birthday
;;       org-bbdb-default-anniversary-format "birthday"
;;       org-bbdb-anniversary-format-alist
;;       '(("birthday" lambda
;;          (name years suffix)
;;          (concat "Birthday: [[bbdb:" name "][" name " ("
;;                  (format "%s" years)
;;                  suffix ")]]"))
;;         ("wedding" lambda
;;          (name years suffix)
;;          (concat "[[bbdb:" name "][" name "'s "
;;                  (format "%s" years)
;;                  suffix " wedding anniversary]]")))
;;       )

;; - put `%%(org-bbdb-anniversaries)' in one of my agenda files. and set
;;   headline with property (:CATEGORY: Anniv)
;;
;; - [C-c C-x p] to set property
;;
;; - select CATEGORY property, value is "`Anniv'".
;;
;; - put this line into agenda file below the
;;   headline. %%(org-bbdb-anniversaries).



;;; [ org-ref ] -- citations, cross-references, indexes, glossaries and bibtex utilities for Org-mode.

(use-package org-ref
  :ensure t
  :defer t
  :init
  (setq org-ref-bibtex-hydra-key-binding (kbd "C-c C-]"))

  (unless (boundp 'org-ref-prefix)
    (define-prefix-command 'org-ref-prefix))
  (define-key my-org-prefix (kbd "C-]") 'org-ref-prefix)

  (define-key org-ref-prefix (kbd "C-]") 'org-ref-insert-link)
  (define-key org-ref-prefix (kbd "c") 'org-ref-helm-insert-cite-link)
  (define-key org-ref-prefix (kbd "l") 'org-ref-helm-insert-label-link)
  (define-key org-ref-prefix (kbd "r") 'org-ref-helm-insert-ref-link)

  :config
  (setq bibtex-completion-pdf-open-function 'org-open-file)

  (setq org-latex-prefer-user-labels t)

  ;; Let org-mode auto process the LaTeX export to PDF process.
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))
  )


;;; [ helm-org-rifle ] -- Rifle through your Org buffers and acquire your target.

(use-package helm-org-rifle
  :ensure t
  :defer t
  :init
  (define-key my-org-prefix (kbd "g") 'helm-org-rifle-current-buffer)
  (define-key my-org-prefix (kbd "G") 'helm-org-rifle)
  :config
  (setq helm-org-rifle-show-path t
        helm-org-rifle-fontify-headings t
        helm-org-rifle-show-todo-keywords t
        helm-org-rifle-show-tags t)
  )


;;; [ otama ] -- Simple org-table based database, intended to be a light version of BBDB and helm-friendly.

;; (use-package otama
;;   :ensure t
;;   :defer t
;;   :init
;;   (setq otama-database-file-name
;;         (concat (getenv "HOME") "/Org" "/otama/otama.org"))
;;
;;   (define-key my-org-prefix (kbd "D") 'otama-helm)
;;   )


;;; [ org-eww ] -- automatically use eww to preview current org-file when save.

;; (use-package org-eww
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'org-mode-hook 'org-eww-mode)
;;   )


(provide 'init-my-org-extensions)

;;; init-my-org-extensions.el ends here
