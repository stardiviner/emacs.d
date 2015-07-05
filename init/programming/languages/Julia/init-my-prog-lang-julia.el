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
;; - `inferior-julia'

(require 'ess-julia)
(autoload 'julia-mode "ess-julia" "Julia mode" t)

;; (setq inferior-julia-args)

(defun my-inferior-julia (&optional process-buffer-name)
  "Start or switch to inferior-julia process buffer PROCESS-BUFFER-NAME."
  (interactive)
  (if (get-buffer-process (or process-buffer-name "*Julia*"))
      ;; the inferior julia process exist
      (switch-to-buffer (or process-buffer-name "*Julia*"))
    ;; create a new inferior julia process
    (inferior-julia)
    ;; (julia)
    ;; kill old process
    ;; (kill-process (get-buffer-process (or process-buffer-name "*julia*"))
    )
  )

(define-key my-inferior-ess-map (kbd "j") 'my-inferior-julia) ; 'julia, 'inferior-julia,

(add-hook 'julia-mode-hook
          (lambda ()
            ;; add julia-mode to prog-mode.
            (unless (derived-mode-p 'prog-mode)
              (run-hooks 'prog-mode-hook))
            ))

;; FIXME:
;; (add-hook 'julia-mode-hook
;;           (lambda ()
;;             (add-to-list (make-local-variable 'company-backends)
;;                          'company-julia-objects)))



;;; fix "flycheck + lintr" error

(add-hook 'julia-mode-hook
          (lambda ()
            (flycheck-mode -1)))


(provide 'init-my-prog-lang-julia)

;;; init-my-prog-lang-julia.el ends here
