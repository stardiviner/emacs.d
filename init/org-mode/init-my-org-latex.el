;;; init-my-org-latex.el --- init for Org LaTeX
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; Embedded LaTeX

;; FIXME: error: Can't preview LaTeX fragment in a non-file buffer.
;; `org-toggle-inline-images' [C-c C-x C-v]
(setq org-startup-with-latex-preview t)
;; `org-toggle-latex-fragment' [C-c C-x C-l] / [C-c C-c]
;; `org-toggle-pretty-entities' [C-c C-x \]

;; config org latex preview style
(setq org-latex-create-formula-image-program 'dvipng
      org-latex-preview-ltxpng-directory "latexpng/"
      org-format-latex-options (plist-put
                                org-format-latex-options :scale 1.5)
      org-format-latex-options (plist-put
                                org-format-latex-options :html-scale 2.0)
      )

;; (setq org-latex-default-packages-alist
;;       org-latex-packages-alist)

;; org -> latex packages
;; (source code format, and syntax color highlighting)
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))

;; (org-toggle-latex-fragment)
;; (setq org-export-filter-latex-fragment-functions nil)

;; (add-hook 'org-mode-hook 'org-toggle-latex-fragment)


(setq org-babel-latex-htlatex t)


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

(setq org-latex-create-formula-image-program 'dvipng)

;;; Using CDLaTeX to enter Math
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)


;;; MathJax
;;
;; ;; CDLaTeX minor mode to speed up math input.
;; (autoload 'cdlatex-mode "cdlatex" nil)
;; ;; enable `org-cdlatex-mode' for all org files
;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)




(provide 'init-my-org-latex)

;;; init-my-org-latex.el ends here
