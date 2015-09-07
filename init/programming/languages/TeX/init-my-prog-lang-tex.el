;;; init-my-prog-lang-tex.el --- init TeX/LaTeX for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;;_ Code:

;;;_ [ AUCTeX ] -- Integrated environment for TeX.

;;;_* Usage:
;;
;; - [C-c C-p] :: preview prefix.
;; - [C-c C-c] :: compile.
;; - [C-c C-v] :: view the paper.
;; - [C-c `] :: popup compile error.
;; - [C-c C-e] :: easy to enter environments \begin{...}  ...  \end{...}

;; LaTeX/P mode defined in `tex-site.el':
;; Major mode in AUCTeX for editing LaTeX files.
;; See info under AUCTeX for full documentation.

(require 'tex-site)


;;;_ [ company-auctex ]

;; (company-auctex-init)
;;
(dolist (hook '(tex-mode-hook
                latex-mode-hook
                ))
  (add-hook hook '(lambda ()
                    (add-to-list (make-local-variable 'company-backends)
                                 'company-auctex-labels)
                    (add-to-list (make-local-variable 'company-backends)
                                 'company-auctex-bibs)
                    (add-to-list (make-local-variable 'company-backends)
                                 '(company-auctex-macros company-auctex-symbols company-auctex-environments))
                    )))


;;;_ [ auto-complete-latex ]

;; (require 'auto-complete-latex)
;;
;; (setq ac-l-dict-directory "~/.emacs.d/el-get/auto-complete-latex/ac-l-dict/")
;;
;; (add-to-list 'ac-modes 'latex-mode)
;;
;; (dolist (hook '(latex-mode-hook
;;                 LaTeX-mode-hook
;;                 ))
;;   (add-hook hook 'ac-l-setup))


;;; [ ac-math ]

;; (require 'ac-math)

;; TODO: https://github.com/vspinu/ac-math
;; https://github.com/vspinu/math-symbol-lists


;;; [ company-math ]

;; TODO: https://github.com/vspinu/company-math


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

(latex-preview-pane-enable)


;;;_ [ reftex ]




;;;_ provide
(provide 'init-my-prog-lang-tex)

;;; init-my-prog-lang-tex.el ends here
