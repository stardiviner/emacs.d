;;; init-prog-lang-racket.el --- init for Racket

;;; Time-stamp: <2019-08-09 08:52:43 stardiviner>

;;; Commentary:



;;; Code:

;;; [ racket-mode ] -- Major mode for Racket language.

(use-package racket-mode
  :ensure t
  :mode (("\\.rkt\\'" . racket-mode)))

;;; [ ob-racket ]

(leaf ob-racket
  :el-get (ob-racket :url "https://github.com/DEADB17/ob-racket.git")
  :commands (org-babel-execute:racket)
  :config
  (add-to-list 'org-babel-load-languages '(racket . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("racket" . "rkt")))



(provide 'init-prog-lang-racket)

;;; init-prog-lang-racket.el ends here
