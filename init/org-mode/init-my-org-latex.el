;;; init-my-org-latex.el --- init for Org LaTeX
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; Embedded LaTeX

;; `org-toggle-inline-images' [C-c C-x C-v]
;; `org-toggle-latex-fragment' [C-c C-x C-l] / [C-c C-c]
;; `org-toggle-pretty-entities' [C-c C-x \]

(setq org-startup-with-latex-preview t)

;; config org latex preview style
(setq org-latex-create-formula-image-program 'dvipng
      ;; org-latex-preview-ltxpng-directory "ltxpng/"
      org-format-latex-options (plist-put
                                org-format-latex-options :scale 1.5)
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
(add-to-list 'org-latex-packages-alist '("" "newfloat" nil))
(add-to-list 'org-latex-packages-alist '("" "minted" nil))
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        )
      )
;; (setq org-latex-minted-options
;;       '(("bgcolor" "bg") ("frame" "lines")))

;; (setq org-format-latex-header)
;; (setq org-format-latex-options
;;       '(:foreground default
;;                     :background default
;;                     :scale 1.0
;;                     :html-foreground "Black"
;;                     :html-background "Transparent"
;;                     :html-scale 1.0
;;                     :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")
;;                     )
;;       )


;;; [ Math ]

;; `helm-insert-latex-math'

;;;_* Math formula support

;;; Using CDLaTeX to enter Math
;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)


;;; MathJax
;;
;; ;; CDLaTeX minor mode to speed up math input.
;; (autoload 'cdlatex-mode "cdlatex" nil)
;; ;; enable `org-cdlatex-mode' for all org files
;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)




(provide 'init-my-org-latex)

;;; init-my-org-latex.el ends here
