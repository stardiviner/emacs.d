;;; init-my-math-wolfram.el --- init for Wolfram
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ wolfram-mode ] -- Mathematica editing and inferior mode.

(use-package wolfram-mode
  :ensure t
  :defer t
  :mode ("\\.m$" . wolfram-mode)
  :config
  (setq wolfram-program "tungsten"
        ;; wolfram-program-arguments
        wolfram-indent 4
        )
  )

;;; [ wolfram.el ] -- Wolfram Alpha integration by showing result with Org-mode.

(use-package wolfram
  :ensure t
  :defer t
  :bind (:map my-prog-help-document-map
              ("A" . wolfram-alpha))
  :config
  (setq wolfram-alpha-app-id "YX2WUR-2J7GPTXY44")
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-math-wolfram)

;;; init-my-math-wolfram.el ends here
