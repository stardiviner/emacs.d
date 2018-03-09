;;; init-org-latex.el --- init for Org LaTeX
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; Embedded LaTeX

;;; Code:


(setq org-startup-with-latex-preview nil)

;; highlight inline LaTeX, and org-entities with different face.
(setq org-highlight-latex-and-related '(latex entities))

;; fix org-mode startup latex preview invalid on non-file buffer.
;; (defun disable-org-latex-preview-on-nonfile ()
;;   (interactive)
;;   (if (not (buffer-file-name))
;;       (setq-local org-startup-with-latex-preview nil)
;;     (setq org-startup-with-latex-preview t))
;;   )
;;
;; (add-hook 'org-mode-hook #'disable-org-latex-preview-on-nonfile)


;;; [ ob-latex ]
(require 'ob-latex)

(add-to-list 'org-babel-load-languages '(latex . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("latex" . "tex"))

;; generate results as #+BEGIN_LaTeX ... #+END_LaTeX block.
;; (setq org-babel-default-header-args:latex
;;       '((:results . "latex")
;;         (:exports . "results")
;;         ))

;; let latex babel generate image result
;; (setq org-babel-default-header-args:latex
;;       '((:results . "raw graphics")
;;         (:file . "temp.png")))


;;; org -> latex packages
;; (add-to-list 'org-latex-default-packages-alist)
;; (add-to-list 'org-latex-packages-alist)

;; (source code format, and syntax color highlighting)

;; 1. use "listings" + "color"
;; (add-to-list 'org-latex-packages-alist '("" "listings"))
;; (add-to-list 'org-latex-packages-alist '("" "color"))

;; 2. use "minted"
;; (setq org-latex-listings 'minted)
;; ;;; enable source code wrap with `breaklines'.
;; (setq org-latex-listings-options '(("breaklines" "true")
;;                                    ("breakanywhere" "true")))
;; ;;; add packages to list.
;; (add-to-list 'org-latex-packages-alist '("" "newfloat" nil))
;; (add-to-list 'org-latex-packages-alist '("" "minted" nil))
;; (setq org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "bibtex %b"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         ))
;;
;; or:
;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
(setq org-latex-minted-options
      '(("bgcolor" "bg")
        ("frame" "lines")
        ("linenos" "true")))

;;; Use `xelatex' to handle Chinese LaTeX.
(setq org-latex-pdf-process '(("latexmk -xelatex -g -pdf %b.tex"
                               "bibtex %b"
                               "latexmk -xelatex -g -pdf %b.tex"
                               "latexmk -xelatex -g -pdf %b.tex")
                              ("xelatex -interaction nonstopmode -output-directory %o %f")))

;; (setq org-format-latex-header)

;;; set LaTeX export to HTML style.
(setq org-format-latex-options
      (plist-put org-format-latex-options :foreground 'default))
(setq org-format-latex-options
      (plist-put org-format-latex-options :background 'default))
(setq org-format-latex-options
      (plist-put org-format-latex-options :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))
(setq org-format-latex-options
      (plist-put org-format-latex-options :html-foreground "Black"))
(setq org-format-latex-options
      (plist-put org-format-latex-options :html-background "Transparent"))
;;; Preview Org-mode LaTeX fragments
;; (setq org-preview-latex-default-process 'dvipng)    ; faster but don't support Chinese by default.
(setq org-preview-latex-default-process 'imagemagick)  ; slower but support Chinese by default.
(setq org-latex-image-default-width "2.0\\linewidth"
      ;; org-latex-image-default-height "20.0\\lineheight"
      )
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.0)) ; adjust LaTeX preview image size.
(setq org-format-latex-options
      (plist-put org-format-latex-options :html-scale 2.5)) ; adjust HTML exporting LaTeX image size.

;;; emabedded latex (inline formula)


;;; [ Math ]

;;; LaTeX Math Symbols

;; `helm-insert-latex-math'

;;;_* Math formula support

;;; Using CDLaTeX to enter Math
;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; ;; CDLaTeX minor mode to speed up math input.
;; (autoload 'cdlatex-mode "cdlatex" nil)
;; ;; enable `org-cdlatex-mode' for all org files
;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;;; change default `org-cdlatex' keybindings.
;; (with-eval-after-load 'org
;;   (define-key org-cdlatex-mode-map (kbd "`") nil)
;;   (setq cdlatex-math-symbol-prefix ?`)
;;   (setq cdlatex-math-modify-prefix ?')
;;   (org-defkey org-cdlatex-mode-map (kbd "\"") 'cdlatex-math-symbol)
;;   (org-defkey org-cdlatex-mode-map (kbd "'") 'org-cdlatex-math-modify))

;;; [ MathJax ]

;; (setq org-html-mathjax-options)


;;; [ org-edit-latex ] -- Org edit LaTeX block/inline like babel src block.

(use-package org-edit-latex
  :ensure t
  :preface (setq org-edit-latex-create-master nil)
  :init (add-hook 'org-mode-hook #'org-edit-latex-mode))


;;; Org-mode export to -> Chinese TeX (ctex) -> PDF

;;; [ org2ctex ] -- Export org to ctex (a latex macro for Chinese)

(use-package org2ctex
  :ensure t
  :config
  (org2ctex-toggle t))


(provide 'init-org-latex)

;;; init-org-latex.el ends here
