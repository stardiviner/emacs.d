;;; org-latex-exp-conf.el --- advice settings around org-latex exporting.

;;; Commentary:



;;; Code:

;;; store default value
(setq-default org-format-latex-header org-format-latex-header)
(setq-default org-latex-packages-alist org-latex-packages-alist)

(make-variable-buffer-local 'org-format-latex-header)
(make-variable-buffer-local 'org-latex-packages-alist)
(make-variable-buffer-local 'org-latex-pdf-process)

(defun org-latex-exp-conf-append (option setting)
  "Append `SETTING' to `OPTION'."
  (cl-case option
    ('org-format-latex-header
     ;; modify variables in buffer local.
     (setq org-format-latex-header
           (concat org-format-latex-header "\n" setting)))
    ('org-latex-packages-alist
     (add-to-list 'org-latex-packages-alist setting))
    ('org-latex-pdf-process
     (add-to-list 'org-latex-pdf-process setting))
    ('org-latex-minted-options
     (add-to-list 'org-latex-minted-options setting))))

(defun org-latex-exp-conf-enable ()
  "Enable org-latex-exp-conf."
  (make-local-variable 'org-format-latex-header)
  (make-local-variable 'org-latex-packages-alist)

  (setq-local org-latex-packages-alist nil)

  ;; Org export to LaTeX default headers.
  ;; set LaTeX default font
  (org-latex-exp-conf-append 'org-format-latex-header "\\setmainfont{DejaVu Sans}")
  (org-latex-exp-conf-append 'org-format-latex-header "\\setsansfont{DejaVu Serif}")
  (org-latex-exp-conf-append 'org-format-latex-header "\\setmonofont{DejaVu Sans Mono}")
  ;; use `fontenc'
  ;; https://orgmode.org/worg/org-tutorials/org-latex-export.html#org135e5d9
  (org-latex-exp-conf-append 'org-format-latex-header "\\usepackage[T1]{fontenc}")
  ;; font: Bera
  (org-latex-exp-conf-append 'org-format-latex-header "\\usepackage[scale]{beraserif}")
  (org-latex-exp-conf-append 'org-format-latex-header "\\usepackage[scale]{berasans}")
  (org-latex-exp-conf-append 'org-format-latex-header "\\usepackage[scale]{beramono}")

  ;; export to PDF with src blocks syntax highlighting.
  (setq-local org-latex-listings 'minted)
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "minted"))
  (setq-local org-latex-minted-options
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

  ;; set default LaTeX engine to xetex
  (setq-local TeX-engine 'xetex)

  ;; set latex to xelatex engine.
  (setq-local org-latex-pdf-process nil) ; reset to nil to avoid other latex engine override "xetex".
  (org-latex-exp-conf-append
   'org-latex-pdf-process
   "xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f")

  (org-latex-exp-conf-append
   'org-latex-pdf-process
   "latexmk -shell-escape -bibtex -xelatex -g -f %f")

  ;; Org-mode export to -> Chinese TeX (ctex) -> PDF
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "ctex"))
  (org-latex-exp-conf-append 'org-format-latex-header "\\usepackage[utf8]{ctex}")
  ;; (org-latex-exp-conf-append 'org-format-latex-header "\\usepackage{xeCJK}")

  ;; specify src block syntax highlighting color scheme
  (org-latex-exp-conf-append 'org-format-latex-header "\\usemintedstyle{manni}")
  ;; set src block
  (org-latex-exp-conf-append
   'org-format-latex-header
   "\\lstset{frame=shadowbox,
numbers=left,
numberstyle= \\tiny,
keywordstyle= \\color{ blue!70},commentstyle=\\color{red!50!green!50!blue!50},
rulesepcolor= \\color{ red!20!green!20!blue!20}"
   )

  ;; highlight special code tags in comments and docstrings (XXX, TODO, BUG, NOTE)
  ;; FIXME:
  ;; (org-latex-exp-conf-append 'org-latex-minted-options '("codetagify" "{TODO, BUG, NOTE, XXX}"))

  ;; support Org-mode Babel coderef (annotate marker on src code block)
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "tikz"))
  (org-latex-exp-conf-append 'org-format-latex-header "\\usepackage{tikz}")
  (org-latex-exp-conf-append 'org-format-latex-header "\\usetikzlibrary{tikzmark,arrows}")
  (org-latex-exp-conf-append
   'org-format-latex-header
   "\\usetikzlibrary{calc,trees,positioning,arrows,chains,shapes.geometric,%
        decorations.pathreplacing,decorations.pathmorphing,shapes,%
        matrix,shapes.symbols}"
   )
  ;; for escape |$\tikzmark{too}$| in src block to interpreted by minted.
  (org-latex-exp-conf-append 'org-latex-minted-options '("escapeinside" "||"))

  ;; some packages used for export LaTeX to PDF.
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "fontspec"))
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "xcolor"))
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "indentfirst"))
  (org-latex-exp-conf-append 'org-format-latex-header "\\usepackage{indentfirst}")
  (org-latex-exp-conf-append 'org-format-latex-header "\\setlength{\parindent}{2em}")
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "xunicode"))
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "float"))
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "longtable"))
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "textcomp"))
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "amsmath"))
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "tabularx"))
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "booktabs"))
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "grffile"))
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "wrapfig"))
  (org-latex-exp-conf-append 'org-latex-packages-alist '("normalem" "ulem"))
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "amssymb"))
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "capt-of"))
  (org-latex-exp-conf-append 'org-latex-packages-alist '("figuresright" "rotating"))
  (org-latex-exp-conf-append 'org-latex-packages-alist '("Lenny" "fncychap"))

  ;; support for export Chinese LaTeX to PDF
  (setf org-latex-default-packages-alist
        (remove '("AUTO" "inputenc" t ("pdflatex")) org-latex-default-packages-alist))
  ;; set font for Chinese
  (org-latex-exp-conf-append 'org-format-latex-header
                             "\\usepackage{fontspec}")
  (org-latex-exp-conf-append 'org-format-latex-header
                             "\\setCJKmainfont[scale=0.6]{WenQuanYi Micro Hei}")
  (org-latex-exp-conf-append 'org-format-latex-header
                             "\\setCJKsansfont{WenQuanYi Micro Hei}")
  (org-latex-exp-conf-append 'org-format-latex-header
                             "\\setCJKmonofont{WenQuanYi Micro Hei Mono}")
  ;; set PDF file default font size. (Chinese)
  ;; (setq org-format-latex-header
  ;;       (replace-regexp-in-string
  ;;        "documentclass{article}"
  ;;        "documentclass[fontsize=5pt]{article}"
  ;;        org-format-latex-header))
  ;; Chinese linebreak
  (org-latex-exp-conf-append 'org-format-latex-header
                             "\\XeTeXlinebreaklocale \"zh\"")
  (org-latex-exp-conf-append 'org-format-latex-header
                             "\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt")
  ;; CJKulem
  (org-latex-exp-conf-append 'org-format-latex-header
                             "\\usepackage{CJKulem}")
  ;; page style
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "fancyhdr"))
  (org-latex-exp-conf-append 'org-format-latex-header "\\pagestyle{fancy}")
  ;; set page fringe width
  (org-latex-exp-conf-append 'org-latex-packages-alist '("" "geometry"))
  (org-latex-exp-conf-append 'org-format-latex-header
                             "\\usepackage[top=2cm, bottom=2cm, left=2cm, right=2cm]{geometry}")
  ;; use some LaTeX macro packages
  (org-latex-exp-conf-append 'org-format-latex-header
                             "\\usepackage{graphicx}")

  (message "`org-latex-exp-conf-mode' enabled."))

(defun org-latex-exp-conf-disable ()
  "Disable org-latex-exp-conf."
  ;; revert variables to default value.
  (setq-local org-format-latex-header (default-value 'org-format-latex-header))
  (setq-local org-latex-packages-alist (default-value 'org-latex-packages-alist))
  (message "`org-latex-exp-conf-mode' disabled."))

(defvar org-latex-exp-conf-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "`org-latex-exp-conf-mode' map.")

(define-minor-mode org-latex-exp-conf-mode
  "This is a minor mode to change org-latex exporting settings temporarily."
  :init-value nil
  :group 'org-export
  :keymap 'org-latex-exp-conf-mode-map
  :global t
  (if org-latex-exp-conf-mode
      (org-latex-exp-conf-enable)
    (org-latex-exp-conf-disable)))



(provide 'org-latex-exp-conf)

;;; org-latex-exp-conf.el ends here
