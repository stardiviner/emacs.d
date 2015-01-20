;;; init-my-prog-lang-tex.el --- init TeX/LaTeX for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;;_ Code:

;;;_ [ AUCTeX ] -- Integrated environment for TeX.

;;;_* Usage:
;;
;; [C-c C-p] :: preview prefix.

;; LaTeX/P mode defined in `tex-site.el':
;; Major mode in AUCTeX for editing LaTeX files.
;; See info under AUCTeX for full documentation.

(require 'tex-site)


;;;_ [ auto-complete-auctex ]

(require 'auto-complete-auctex)


;;;_ [ company-auctex ]

;; (require 'company-auctex)
;; (company-auctex-init)


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


;;;_ [ reftex ]




;;;_ provide
(provide 'init-my-prog-lang-tex)

;;; init-my-prog-lang-tex.el ends here
