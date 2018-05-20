;;; init-org-latex.el --- init for Org LaTeX
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; Embedded LaTeX

;;; Code:

(require 'ox-latex)

;; highlight inline LaTeX, and org-entities with different face.
(setq org-highlight-latex-and-related '(latex entities))

;; let org-mode auto delete those auxiliary files after exporting.
(setq org-latex-remove-logfiles t)
(setq org-latex-logfiles-extensions
      '("lof" "lot" "tex" "aux" "idx" "out" "toc" "nav" "snm" "vrb"
        "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"))

;;; Preview Org-mode LaTeX fragments
(setq org-startup-with-latex-preview nil)
;; (setq org-preview-latex-default-process 'dvipng)    ; faster but don't support Chinese by default.
;; (setq org-preview-latex-default-process 'imagemagick)  ; slower but support Chinese by default.
(setq org-preview-latex-default-process 'dvisvgm) ; generate SVG for better image.

;; (setq org-preview-latex-default-process 'dvisvgm-xetex)
;; (add-to-list 'org-preview-latex-process-alist
;;              '(dvisvgm-xetex
;;                :programs ("xetex" "dvisvgm")
;;                :description "dvi > svg"
;;                :message "you need to install the programs: latex and dvisvgm."
;;                :use-xcolor t
;;                :image-input-type "dvi"
;;                :image-output-type "svg"
;;                :image-size-adjust (1.7 . 1.5)
;;                :latex-compiler ("xelatex -8bit --shell-escape -interaction=nonstopmode -output-directory %o %f")
;;                :image-converter ("dvisvgm %f -n -b min -c %S -o %O")))

(setq org-latex-image-default-width "1.5\\linewidth")
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 1.5)) ; adjust LaTeX preview image size.
(setq org-format-latex-options
      (plist-put org-format-latex-options :html-scale 2.0)) ; adjust HTML exporting LaTeX image size.

(setq org-latex-prefer-user-labels t)

;;; [ webkit-katex-render ] -- An instant Latex Previewer for Emacs.

;; (use-package webkit-katex-render
;;   ;; :ensure t
;;   :load-path "~/Code/Emacs/webkit-katex-render/"
;;   :init (add-hook 'org-mode-hook #'webkit-katex-render-mode)
;;   ;; (add-hook 'TeX-mode-hook #'webkit-katex-render-mode)
;;   )

;;; [ Exporting ]
(require 'org-latex-exp-conf)
;; (define-key org-mode-map (kbd "C-c M-e") 'org-latex-exp-conf-mode)

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

;; (setq org-html-mathjax-options
;;       '((path "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML")
;;         (scale "100")
;;         (align "center")
;;         (font "TeX")
;;         (linebreaks "false")
;;         (autonumber "AMS")
;;         (indent "0em")
;;         (multlinewidth "85%")
;;         (tagindent ".8em")
;;         (tagside "right"))
;;       )

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
