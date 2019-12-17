;;; init-org-latex.el --- init for Org LaTeX
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; Embedded LaTeX

;;; Code:

(use-package ox-latex
  :defer t
  :init
  ;; let org-mode auto delete those auxiliary files after compile and exporting.
  (setq org-latex-remove-logfiles t)
  (setq org-latex-logfiles-extensions
        '("lof" "lot" "aux" "idx" "out" "toc" "nav" "snm" "vrb"
          "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"))

  ;; Preview Org-mode LaTeX fragments between $\LaTeX{}$
  (setq org-startup-with-latex-preview t)
  (setq org-highlight-latex-and-related '(native latex script))

  ;; (setq org-preview-latex-default-process 'dvipng)    ; faster but don't support Chinese by default.
  ;; (setq org-preview-latex-default-process 'imagemagick)  ; slower but support Chinese by default.
  (setq org-preview-latex-default-process 'dvisvgm) ; generate SVG for better image.

  (setq org-latex-create-formula-image-program 'dvisvgm)
;;; FIXME:
  ;; (setq org-latex-pdf-process (my-org-latex-pdf-process-format))
  ;; (defun my-org-latex-pdf-process-format (&optional texfile snippet caller-info)
  ;;   (princ (format "The caller's info: %s" caller-info))
  ;;   (cond
  ;;    (snippet '("latex -interaction nonstopmode -output-directory %o %f"))
  ;;    (t '("%latex -interaction nonstopmode -output-directory %o %f"
  ;;         "%latex -interaction nonstopmode -output-directory %o %f"
  ;;         "%latex -interaction nonstopmode -output-directory %o %f"))))

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

  (setq org-latex-prefer-user-labels t))

;;; [ webkit-katex-render ] -- An instant Latex Previewer for Emacs.

;; (use-package webkit-katex-render
;;   ;; :ensure t
;;   :load-path "~/Code/Emacs/webkit-katex-render/"
;;   :init (add-hook 'org-mode-hook #'webkit-katex-render-mode)
;;   ;; (add-hook 'TeX-mode-hook #'webkit-katex-render-mode)
;;   )

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
  :defer t
  :delight org-edit-latex-mode
  :preface (setq org-edit-latex-create-master nil)
  :init (add-hook 'org-mode-hook #'org-edit-latex-mode))


;;; [ ob-latex ] -- Babel Functions for LaTeX.

(use-package ob-latex
  :defer t
  :commands (org-babel-execute:latex)
  :config
  (add-to-list 'org-babel-load-languages '(latex . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;;; [ org-ref ] -- citations, cross-references and bibliographies in Org Mode.

(use-package org-ref
  :ensure t
  :defer t
  :commands (org-ref)
  :init
  (setq bibtex-completion-pdf-open-function 'org-open-file)
  (setq org-latex-prefer-user-labels t)
  (setq org-ref-bibtex-hydra-key-binding "\C-cj")

  (unless (boundp 'org-ref-prefix)
    (define-prefix-command 'org-ref-prefix))
  (define-key Org-prefix (kbd "C-]") 'org-ref-prefix)

  (define-key org-ref-prefix (kbd "C-]") 'org-ref-insert-link)
  (define-key org-ref-prefix (kbd "c") 'org-ref-helm-insert-cite-link)
  (define-key org-ref-prefix (kbd "l") 'org-ref-helm-insert-label-link)
  (define-key org-ref-prefix (kbd "r") 'org-ref-helm-insert-ref-link)

  ;; Let org-mode auto process the LaTeX export to PDF process.
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f")))


(provide 'init-org-latex)

;;; init-org-latex.el ends here
