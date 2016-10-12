;;; init-my-math-wolfram.el --- init for Wolfram
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------

(use-package wolfram-mode
  :ensure t
  :init
  (autoload 'wolfram-mode "wolfram-mode" nil t)
  (autoload 'run-wolfram "wolfram-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.m$" . wolfram-mode))
  :config
  (setq wolfram-program "tungsten"
        ;; wolfram-program-arguments
        wolfram-indent 4
        )
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-math-wolfram)

;;; init-my-math-wolfram.el ends here
