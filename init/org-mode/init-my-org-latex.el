;;; init-my-org-latex.el --- init for Org LaTeX
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; Embedded LaTeX

;; `org-toggle-inline-images' [C-c C-x C-v]
;; `org-toggle-latex-fragment' [C-c C-x C-l] / [C-c C-c]
;; `org-toggle-pretty-entities' [C-c C-x \]

(setq org-startup-with-latex-preview nil)

;; fix org-mode startup latex preview invalid on non-file buffer.
;; (defun disable-org-latex-preview-on-nonfile ()
;;   (interactive)
;;   (if (not (buffer-file-name))
;;       (setq-local org-startup-with-latex-preview nil)
;;     (setq org-startup-with-latex-preview t))
;;   )
;;
;; (add-hook 'org-mode-hook #'disable-org-latex-preview-on-nonfile)

;; config org latex preview style
(setq org-latex-create-formula-image-program 'dvipng
      ;; org-latex-preview-ltxpng-directory "ltxpng/"
      ;; set latex fragment preview image size
      org-format-latex-options (plist-put
                                org-format-latex-options :scale 2.0)
      org-format-latex-options (plist-put
                                org-format-latex-options :html-scale 2.0)
      )

;; (setq org-latex-default-packages-alist
;;       org-latex-packages-alist)

;;; org -> latex packages
;; (source code format, and syntax color highlighting)
;;
(require 'ox-latex)

;; 1. use "listings" + "color"
;; (add-to-list 'org-latex-packages-alist '("" "listings"))
;; (add-to-list 'org-latex-packages-alist '("" "color"))

;; 2. use "minted"
(setq org-latex-listings 'minted)
;;; enable source code wrap with `breaklines'.
(setq org-latex-listings-options '(("breaklines" "true")
                                   ("breakanywhere" "true")))
;;; add packages to list.
(add-to-list 'org-latex-packages-alist '("" "newfloat" nil))
(add-to-list 'org-latex-packages-alist '("" "minted" nil))
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        ))
(setq org-latex-minted-options
      '(("bgcolor" "bg")
        ("frame" "lines")
        ("linenos" "true")))

;; (setq org-format-latex-header)

(setq org-format-latex-options
      '(:foreground default
                    :background default
                    :scale 1.5
                    :html-foreground "Black"
                    :html-background "Transparent"
                    :html-scale 1.0
                    :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")
                    )
      )


;;; [ Math ]




;;; LaTeX Math Symbols

;; `helm-insert-latex-math'

;;;_* Math formula support

;;; Using CDLaTeX to enter Math
;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)


;;; MathJax

;; (setq org-html-mathjax-options)

;; ;; CDLaTeX minor mode to speed up math input.
;; (autoload 'cdlatex-mode "cdlatex" nil)
;; ;; enable `org-cdlatex-mode' for all org files
;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)


;;; emabedded latex (inline formula)

(defface org-latex-face
  (org-compatible-face 'shadow
    '((((class color grayscale) (min-colors 88) (background light))
       (:foreground "black" :background "brown"))
      (((class color grayscale) (min-colors 88) (background dark))
       (:foreground "white" :background "forest green"))
      (((class color) (min-colors 8) (background light))
       (:foreground "black" :background "gray"))
      (((class color) (min-colors 8) (background dark))
       (:foreground "white" :background "gray"))))
  "Face for fixed-width text like code snippets."
  :group 'org-faces
  :version "22.1")


;;; [ org-edit-latex ] -- Org edit LaTeX block like babel src block.

(use-package org-edit-latex
  :ensure t
  :config
  (add-hook 'org-mode-hook #'org-edit-latex-mode))


(provide 'init-my-org-latex)

;;; init-my-org-latex.el ends here
