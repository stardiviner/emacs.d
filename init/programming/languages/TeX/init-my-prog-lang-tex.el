;;; init-my-prog-lang-tex.el --- init TeX/LaTeX for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ TeX ]

(require 'tex)


;;; [ LaTeX ]

(require 'latex)


;;; [ AUCTeX ] -- Integrated environment for TeX.

;; LaTeX/P mode defined in `tex-site.el':
;; Major mode in AUCTeX for editing LaTeX files.
;; See info under AUCTeX for full documentation.
(require 'tex-site)

;;; [C-c C-p] :: preview prefix.


;;; [ auto-complete-auctex ]

(require 'auto-complete-auctex)


;;; [ auto-complete-latex ]



(provide 'init-my-prog-lang-tex)

;;; init-my-prog-lang-tex.el ends here
