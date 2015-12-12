;;; init-my-prog-lang-tex.el --- init TeX/LaTeX for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ tex-mode ]


;;; [ latex-mode ]


;;; [ AUCTeX ] -- Integrated environment for TeX.

;;; Usage:
;;
;; - [C-c C-e] :: easy to enter environments \begin{...}  ...  \end{...}
;; - [C-c C-p] :: preview prefix.
;; - [C-c C-c] :: compile.
;; - [C-c C-v] :: view the paper.
;; - [C-c `] :: popup compile error.

(require 'tex-site)

;;; AUCTeX config
(setq TeX-auto-save t
      TeX-parse-self t)


;;; [ company-auctex ]
;;; [ company-math ]

(dolist (hook '(tex-mode-hook
                latex-mode-hook
                LaTeX-mode-hook ; from AUCTeX
                ))
  (add-hook hook
            '(lambda ()
               ;; indent
               (aggressive-indent-mode)

               ;; fold
               (TeX-fold-mode)

               ;; complete
               (my-company-add-backends-to-mode
                '(company-auctex-macros
                  company-auctex-symbols
                  company-auctex-environments

                  company-auctex-labels
                  company-auctex-bibs
                  
                  ;; company-math
                  company-latex-commands
                  company-math-symbols-latex
                  company-math-symbols-unicode
                  ))

               ;; linter
               (flycheck-mode)


               ;; block
               (local-set-key (kbd "C-c C-i") 'tex-latex-block)
               
               ;; Section
               (setq LaTeX-section-hook
                     '(LaTeX-section-heading
                       LaTeX-section-title
                       LaTeX-section-toc
                       LaTeX-section-section
                       LaTeX-section-label))

               ;; Math
               ;; (LaTeX-math-mode)
               )))


;;; [ latex-pretty-symbols ]

(require 'latex-pretty-symbols)


;;; [ latex-preview-pane ]

;; Usage:
;;
;; - [M-x latex-preview-mode]

;; To use LaTeX Preview Pane, simply open any TeX file and if latex-preview-pane
;; is set to be automatically enabled, it will open a preview pane and attempt
;; to generate your TeX preview. Otherwise you can activate it with M-x
;; latex-preview-pane-mode to open the preview pane. Note that there is also a
;; menu in this mode which contains the following functions:

;; - Refresh Preview (bound to M-p)
;; - Open in External Program (Bound to M-P)
;; - Disable LaTeX Preview Pane (turns the mode off, you can also use M-x
;;   latex-preview-pane-mode to toggle it off.
;; - Customize LaTeX Preview Pane (opens a customization buffer where you can
;;   set the command to use for generating previews)

(use-package latex-preview-pane
  :config
  (latex-preview-pane-enable))


;;; [ px ]

;; Provides functions to preview LaTeX codes like $x^2$ in any
;; buffer/mode.

;; Use `px-preview-region' to preview LaTeX codes delimited by $ pairs
;; in the region.
;; Use `px-preview' to process the whole buffer.
;; Use `px-remove' to remove all images and restore the text back.
;; Use `px-toggle' to toggle between images and text on the whole
;; buffer.

;; Most of this code comes from weechat-latex.el which in turn uses
;; org-mode previewer.

(use-package px)


;;; [ reftex ]




(provide 'init-my-prog-lang-tex)

;;; init-my-prog-lang-tex.el ends here
