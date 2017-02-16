;;; init-my-prog-test-coverage.el --- init for Code Test Coverage

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------

;;; [ coverage ] -- a minor mode to highlight code coverage in source code files.

;; (use-package coverage
;;   :ensure t
;;   :config
;;   (coverage-mode 1)
;;   )

;;; [ coverlay ] -- test coverage overlay for Emacs.

;; (use-package coverlay
;;   :ensure t
;;   :config
;;   (coverlay-mode 1)
;;   )

;;; [ cov ] -- show coverage stats in the fringe.

(use-package cov
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'cov-mode)
  )




;;; ----------------------------------------------------------------------------

(provide 'init-my-prog-test-coverage)

;;; init-my-prog-test-coverage.el ends here
