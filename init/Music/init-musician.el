;;; init-musician.el --- init for Musician.

;;; Commentary:



;;; Code:

;;; [ LilyPond ]

(use-package lilypond-mode
  :preface (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
  :mode (("\\.ly$" . LilyPond-mode)
         ("\\.ily$" . LilyPond-mode))
  :init (add-to-list 'display-buffer-alist '("\\*lilypond\\*" . (display-buffer-below-selected)))
  :hook (LilyPond-mode . turn-on-font-lock))

;;; [ ob-lilypond ]

(use-package ob-lilypond
  :defer t
  :commands (org-babel-execute:lilypond)
  :config
  (add-to-list 'org-babel-load-languages '(lilypond . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  ;; remove large white space in pager
  (add-to-list 'org-babel-default-header-args:lilypond
               '(:prologue . "\\paper{ oddFooterMarkup=##f }")))

;;; [ gregorio-mode ] -- Gregorio Mode for .gabc files.

(use-package gregorio-mode
  :ensure t
  :defer t)



(provide 'init-musician)

;;; init-musician.el ends here
