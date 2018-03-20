;;; init-org-latex.el --- init for Org LaTeX
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; Embedded LaTeX

;;; Code:

(require 'ox-latex)

;; highlight inline LaTeX, and org-entities with different face.
(setq org-highlight-latex-and-related '(latex entities))


;;; Preview Org-mode LaTeX fragments
(setq org-startup-with-latex-preview nil)
;; (setq org-preview-latex-default-process 'dvipng)    ; faster but don't support Chinese by default.
;; (setq org-preview-latex-default-process 'imagemagick)  ; slower but support Chinese by default.
(setq org-preview-latex-default-process 'dvisvgm) ; generate SVG for better image.
(setq org-latex-image-default-width "2.0\\linewidth")
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.0)) ; adjust LaTeX preview image size.
(setq org-format-latex-options
      (plist-put org-format-latex-options :html-scale 2.5)) ; adjust HTML exporting LaTeX image size.


;; (setq org-format-latex-header)

;;; org -> latex packages
;; (add-to-list 'org-latex-default-packages-alist)
;; (add-to-list 'org-latex-packages-alist)


;;; export to PDF with src blocks syntax highlighting.
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("newfloat" "minted" "color"))
(add-to-list 'org-latex-pdf-process
	           "latexmk -shell-escape -bibtex -f -pdf %f")
(setq org-latex-minted-options
      '(("frame" "lines")
        ("linenos" "true")
        ("bgcolor" "bg")
        ))

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


;;; emabedded latex (inline formula)


;;; [ Math ]

;;; LaTeX Math Symbols
;; `helm-insert-latex-math'

;;; Math formula support
;; Using CDLaTeX to enter Math
;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
;;
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


;;; [ ob-latex ]
(require 'ob-latex)

(add-to-list 'org-babel-load-languages '(latex . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)


;;; Org-mode export to -> Chinese TeX (ctex) -> PDF

;;; Use `xelatex' to handle Chinese LaTeX.
(dolist (cmd '("latexmk -xelatex -g -pdf %b.tex"
               "latexmk -xelatex -g -pdf %b.tex"
		           "bibtex %b"
		           "latexmk -xelatex -g -pdf %b.tex"))
  (add-to-list 'org-latex-pdf-process cmd))
(add-to-list 'org-latex-pdf-process
	           "xelatex -interaction nonstopmode -output-directory %o %f")

;;; [ org2ctex ] -- Export org to ctex (a latex macro for Chinese)

(use-package org2ctex
  :ensure t
  :config
  (org2ctex-toggle t))


(provide 'init-org-latex)

;;; init-org-latex.el ends here
