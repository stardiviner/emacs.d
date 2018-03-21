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

;;; Org export to LaTeX default headers.
;; set LaTeX default font
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\setmainfont{DejaVu Sans}"))
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\setsansfont{DejaVu Serif}"))
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\setmonofont{DejaVu Sans Mono}"))


;;; org -> latex packages
;; (add-to-list 'org-latex-default-packages-alist)
;; (add-to-list 'org-latex-packages-alist)

;;; export to PDF with src blocks syntax highlighting.
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-minted-options
      '(("frame" "lines")
        ("linenos" "true") ; enable number lines
        ;; ("frame" "single") ; box frame
        ("escapeinside" "$$") ; escape to LaTeX between the two characters specified in $$.
        ("mathescape" "true") ; escape and interrupt math in src block
        ("texcomments" "true") ; enable LaTeX code inside comments
        ("numbersep" "5pt") ; gap between numbers and start of line
        ("framesep" "2mm") ; distance between frame and content
        ;; ("fontsize" "??") ; font size in code block
        ("breaklines" "true")
        ))
;; (add-to-list 'org-latex-minted-langs '(clojure "Clojure"))
;; (add-to-list 'org-latex-pdf-process
;; 	           "latexmk -shell-escape -bibtex -xelatex -g -f %f")
;;; Org-mode export to -> Chinese TeX (ctex) -> PDF
;;; set default LaTeX engine to xetex
(setq-default TeX-engine 'xetex)
(add-to-list 'org-latex-packages-alist '("" "ctex"))
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\usepackage[utf8]{ctex}"))
;; (setq org-format-latex-header
;;       (concat org-format-latex-header "\n" "\\usepackage{xeCJK}"))
;;; set latex to xelatex engine.
(setq org-latex-pdf-process
		  '("xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
		    "xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
        "xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"))
;; specify src block syntax highlighting color scheme
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\usemintedstyle{manni}"))
;; set src block
(setq org-format-latex-header
      (concat org-format-latex-header "\n"
              "\\lstset{frame=shadowbox,
numbers=left,
numberstyle= \\tiny,
keywordstyle= \\color{ blue!70},commentstyle=\\color{red!50!green!50!blue!50},
rulesepcolor= \\color{ red!20!green!20!blue!20}"))

;;; highlight special code tags in comments and docstrings (XXX, TODO, BUG, NOTE)
;; FIXME:
;; (add-to-list 'org-latex-minted-options '("codetagify" "{TODO, BUG, NOTE, XXX"}"))

;; (setq org-format-latex-header
;; (setq org-format-latex-header
;;; support Org-mode Babel coderef (annotate marker on src code block)
(add-to-list 'org-latex-packages-alist '("" "tikz"))
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\usepackage{tikz}"))
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\usetikzlibrary{tikzmark,arrows}"))
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\usetikzlibrary{calc,trees,positioning,arrows,chains,shapes.geometric,%
        decorations.pathreplacing,decorations.pathmorphing,shapes,%
        matrix,shapes.symbols}"))
;; for escape |$\tikzmark{too}$| in src block to interpreted by minted.
;; (add-to-list 'org-latex-minted-options '("escapeinside" "||"))

;;; some packages used for export LaTeX to PDF.
(add-to-list 'org-latex-packages-alist '("" "fontspec"))
(add-to-list 'org-latex-packages-alist '("" "xcolor"))
(add-to-list 'org-latex-packages-alist '("" "indentfirst"))
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\usepackage{indentfirst}"))
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\setlength{\parindent}{2em}"))
(add-to-list 'org-latex-packages-alist '("" "xunicode"))
(add-to-list 'org-latex-packages-alist '("" "float"))
(add-to-list 'org-latex-packages-alist '("" "longtable"))
(add-to-list 'org-latex-packages-alist '("" "textcomp"))
(add-to-list 'org-latex-packages-alist '("" "amsmath"))
(add-to-list 'org-latex-packages-alist '("" "tabularx"))
(add-to-list 'org-latex-packages-alist '("" "booktabs"))
(add-to-list 'org-latex-packages-alist '("" "grffile"))
(add-to-list 'org-latex-packages-alist '("" "wrapfig"))
(add-to-list 'org-latex-packages-alist '("normalem" "ulem"))
(add-to-list 'org-latex-packages-alist '("" "amssymb"))
(add-to-list 'org-latex-packages-alist '("" "capt-of"))
(add-to-list 'org-latex-packages-alist '("figuresright" "rotating"))
(add-to-list 'org-latex-packages-alist '("Lenny" "fncychap"))

;;; support for export Chinese LaTeX to PDF
(setf org-latex-default-packages-alist
      (remove '("AUTO" "inputenc" t ("pdflatex")) org-latex-default-packages-alist))
;;; set font for Chinese
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\usepackage{fontspec}"))
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\setCJKmainfont[scale=0.6]{WenQuanYi Micro Hei}"))
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\setCJKsansfont{WenQuanYi Micro Hei}"))
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\setCJKmonofont{WenQuanYi Micro Hei Mono}"))
;; set PDF file default font size. (Chinese)
(setq org-format-latex-header
      (replace-regexp-in-string
       "documentclass{article}"
       "documentclass[fontsize=5pt]{article}"
       org-format-latex-header))
;; Chinese linebreak
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\XeTeXlinebreaklocale \"zh\""))
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt"))
;; CJKulem
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\usepackage{CJKulem}"))
;; page style
(add-to-list 'org-latex-packages-alist '("" "fancyhdr"))
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\pagestyle{fancy}"))
;; set page fringe width
(add-to-list 'org-latex-packages-alist '("" "geometry"))
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\usepackage[top=2cm, bottom=2cm, left=2cm, right=2cm]{geometry}"))
;;; use some LaTeX macro packages
(setq org-format-latex-header
      (concat org-format-latex-header "\n" "\\usepackage{graphicx}"))

;; let org-mode auto delete those auxiliary files after exporting.
(setq org-latex-remove-logfiles t)
(setq org-latex-logfiles-extensions
      '("lof" "lot" "tex" "aux" "idx" "out" "toc" "nav" "snm" "vrb"
        "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"))

;;; [ LaTeX -> HTML ]

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


(provide 'init-org-latex)

;;; init-org-latex.el ends here
