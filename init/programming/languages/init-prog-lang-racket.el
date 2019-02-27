;;; init-prog-lang-racket.el --- init for Racket

;;; Time-stamp: <2019-02-27 15:37:17 stardiviner>

;;; Commentary:



;;; Code:

;;; [ racket-mode ] -- Major mode for Racket language.

(use-package racket-mode
  :ensure t
  :mode (("\\.rkt\\'" . racket-mode)))



(provide 'init-prog-lang-racket)

;;; init-prog-lang-racket.el ends here
