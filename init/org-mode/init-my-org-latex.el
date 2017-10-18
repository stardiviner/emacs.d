;;; init-my-org-latex.el --- init for Org LaTeX
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; Embedded LaTeX

;;; Code:

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


;; `org-toggle-inline-images' [C-c C-x C-v]
;; `org-toggle-latex-fragment' [C-c C-x C-l] / [C-c C-c]
;; `org-toggle-pretty-entities' [C-c C-x \]

(setq org-startup-with-latex-preview nil)

;; highlight inline LaTeX with different face.
(setq org-highlight-latex-and-related '(latex script entities))

;; fix org-mode startup latex preview invalid on non-file buffer.
;; (defun disable-org-latex-preview-on-nonfile ()
;;   (interactive)
;;   (if (not (buffer-file-name))
;;       (setq-local org-startup-with-latex-preview nil)
;;     (setq org-startup-with-latex-preview t))
;;   )
;;
;; (add-hook 'org-mode-hook #'disable-org-latex-preview-on-nonfile)


;; (add-to-list 'org-latex-default-packages-alist)
;; (add-to-list 'org-latex-packages-alist)

;;; org -> latex packages
;; (source code format, and syntax color highlighting)

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
      (plist-put org-format-latex-options :foreground 'default))
(setq org-format-latex-options
      (plist-put org-format-latex-options :background 'default))
(setq org-format-latex-options
      (plist-put org-format-latex-options :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))
(setq org-format-latex-options
      (plist-put org-format-latex-options :html-foreground "Black"))
(setq org-format-latex-options
      (plist-put org-format-latex-options :html-background "Transparent"))
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.0))
(setq org-format-latex-options
      (plist-put org-format-latex-options :html-scale 2.5))


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

;;; Preview Org-mode LaTeX fragments
;; (setq org-preview-latex-default-process 'dvipng)    ;速度很快，但 *默认* 不支持中文
(setq org-preview-latex-default-process 'imagemagick)  ;速度较慢，但支持中文
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.0))      ;调整 LaTeX 预览图片的大小
(setq org-format-latex-options
      (plist-put org-format-latex-options :html-scale 2.5)) ;调整 HTML 文件中 LaTeX 图像的大小



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
  (setq org-edit-latex-create-master 'ask)
  
  (add-hook 'org-mode-hook #'org-edit-latex-mode)

  (defun org-edit-latex-active-in-LaTeX-fragment ()
    (if (and (equal major-mode 'org-mode)
             (or (eq 'latex-environment (car (org-element-context)))
                 (org-inside-LaTeX-fragment-p)))
        (progn
          (message "You can use `org-edit-latex' [C-c '].")
          (unless org-edit-latex-mode (org-edit-latex-mode 1)))
      (if org-edit-latex-mode (org-edit-latex-mode -1))
      ))

  (add-hook 'post-command-hook #'org-edit-latex-active-in-LaTeX-fragment)
  )


;;; Org-mode export to -> Chinese TeX (ctex) -> PDF

;;; [ org2ctex ] -- Export org to ctex (a latex macro for Chinese)

(use-package org2ctex
  :ensure t
  :config
  (org2ctex-toggle t)
  )


(provide 'init-my-org-latex)

;;; init-my-org-latex.el ends here
