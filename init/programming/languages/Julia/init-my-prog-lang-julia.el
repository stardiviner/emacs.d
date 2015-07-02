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

;; (define-key my-inferior-ess-map (kbd "j") 'julia)
(define-key my-inferior-ess-map (kbd "j")
  '(lambda ()
     (interactive)
     (if (get-buffer-process "*julia*")
         ;; the inferior julia process exist
         (switch-to-buffer "*julia*")
       ;; create a new inferior julia process
       (julia)
       ;; kill old process
       ;; (kill-process (get-buffer-process "*julia*"))
       )))

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
