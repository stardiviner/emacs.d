;;; init-my-math-wolfram.el --- init for Wolfram
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ wolfram.el ] -- Wolfram Alpha integration by showing result with Org-mode.

(use-package wolfram
  :ensure t
  :defer t
  :bind (:map prog-doc-map
              ("A" . wolfram-alpha))
  :config
  (setq wolfram-alpha-app-id "YX2WUR-2J7GPTXY44")
  )


(provide 'init-my-math-wolfram)

;;; init-my-math-wolfram.el ends here
