;;; init-math-wolfram.el --- init for Wolfram
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ wolfram.el ] -- Wolfram Alpha integration by showing result with Org-mode.

(use-package wolfram
  :ensure t
  :defer t
  :commands (walfram-alpha)
  :bind (:map document-prefix ("A" . wolfram-alpha))
  :init (add-to-list 'display-buffer-alist '("\\*WolframAlpha\\*" . (display-buffer-below-selected)))
  :custom (wolfram-alpha-app-id "YX2WUR-2J7GPTXY44"))

;;; [ wolfram-mode ] -- Mathematica editing and inferior mode.

(use-package wolfram-mode
  :ensure t
  :defer t)


(provide 'init-math-wolfram)

;;; init-math-wolfram.el ends here
