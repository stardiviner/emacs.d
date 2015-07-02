;;; init-my-prog-lang-julia.el --- init for Julia
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ESS - Julia ]


;;; [ julia-mode ]

;;; Usage:
;;
;; - `julia-mode'
;; - `julia-eldoc-function'
;; - `julia-manual-lookup-function'

(require 'julia-mode)


;;; fix "flycheck + lintr" error

(add-hook 'julia-mode-hook
          (lambda ()
            (flycheck-mode -1)))


(provide 'init-my-prog-lang-julia)

;;; init-my-prog-lang-julia.el ends here
