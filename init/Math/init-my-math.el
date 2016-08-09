;;; init-my-math.el --- init for Mathematics
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Proof Assistant ]

(require 'init-my-math-proof-assistant)


;;; [ Sage Math ]

(require 'init-my-math-sage-math)


;;; [ GMPL (MathProg) ] -- GMPL(MathProg) files

(use-package gmpl-mode
  :ensure t)


;;; [ Wolfram ]

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


(provide 'init-my-math)

;;; init-my-math.el ends here
