;;; init-my-prog-lang-julia.el --- init for Julia
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))



;;; [ ESS - Julia ]

;;; Usage:
;;
;; - `julia-mode'
;; - `julia-eldoc-function'
;; - `julia-manual-lookup-function'

(require 'ess-julia)
(autoload 'julia-mode "ess-julia" "Julia mode" t)

(define-key my-inferior-ess-map (kbd "j") 'julia)

;; FIXME:
;; (add-hook 'julia-mode-hook
;;           (lambda ()
;;             (add-to-list (make-local-variable 'company-backends)
;;                          'company-julia-objects)))


;; (unload-feature 'ess-julia)


;;; fix "flycheck + lintr" error

(add-hook 'julia-mode-hook
          (lambda ()
            (flycheck-mode -1)))


(provide 'init-my-prog-lang-julia)

;;; init-my-prog-lang-julia.el ends here
