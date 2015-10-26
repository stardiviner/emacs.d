;;; init-my-prog-lang-julia.el --- init for Julia
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ julia-mode ]

(use-package julia-mode
  :config
  (setq julia-indent-offset 4)
  )


;;; [ ESS - Julia ]

;;; Usage:
;;
;; - [M-x julia RET] / [C-c C-z] :: switch to julia process buffer.
;;         It will show `julia> ESS` represents loaded ESS.
;;   - julia> quit()  :: to quit process.
;; - [C-c C-s] :: associate a buffer with a different julia process.
;;
;; - `julia-mode'
;; - `julia-eldoc-function'
;; - `julia-manual-lookup-function'
;; - `inferior-julia'
;;
;; - Evaluation -- send chunks of code
;;   - [C-c C-c] :: [C-h k C-c C-c]
;;   - [C-M-x]
;;   - [C-c C-l] :: load the whole file.
;; - Help
;;   - [C-c C-d C-h] :: `ess-doc-map'
;;   - [C-c C-d C-d] :: help on any topic or object.
;;   - [C-c C-d C-a] :: help with apropos.
;;   - [C-c C-d C-r] :: standard library reference.
;;   - [C-c C-d m]   :: topic on Julia manual.
;;   - [C-c C-d C-w] :: search julia website.
;; - Error Navigation
;;   - [M-g n/p] :: navigate in error list.
;;   - Julia conveniently reports the location of its own source files. In order
;;     to make ESS to understand these links, add the julia’s source folders to
;;     ess-tracebug-search-path:
;;     (add-to-list 'ess-tracebug-search-path "/path/to/julia/base/")
;; - Imenu
;;   -
;; - Completion
;;   - [C-M-i] / [TAB] :: `ess-tab-complete-in-script'
;; - Eldoc


;; (setq inferior-julia-args)

(add-hook 'julia-mode-hook
          (lambda ()
            ;; add julia-mode to prog-mode.
            (unless (derived-mode-p 'prog-mode)
              (run-hooks 'prog-mode-hook))
            ))

(add-hook 'julia-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-ess-julia-objects)))

(add-hook 'inferior-julia-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-ess-julia-objects)))

(add-hook 'inferior-ess-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-ess-julia-objects)))


(defun my-inferior-julia (&optional process-buffer-name)
  "Start or switch to inferior-julia process buffer PROCESS-BUFFER-NAME."
  (interactive)
  (if (get-buffer-process (or process-buffer-name "*Julia*"))
      ;; the inferior julia process exist
      (switch-to-buffer (or process-buffer-name "*Julia*"))
    ;; create a new inferior julia process
    (julia)
    ;; (julia)
    ;; kill old process
    ;; (kill-process (get-buffer-process (or process-buffer-name "*julia*"))
    )
  )

;; 'julia, 'inferior-julia,
(define-key my-inferior-ess-map (kbd "j") 'my-inferior-julia)


;;; [ julia-shell ] -- inferior Julia

;;; Usage:
;;
;; - [M-x julia-shell] :: to interact with `julia-shell' from `julia-mode'.
;; - `inferior-julia-shell'
;; - `run-julia'

;; (use-package julia-shell
;;   :config
;;   ;; (require 'julia-shell-mode)
;;
;;   ;; FIXME:
;;   ;; (add-hook 'julia-mode-hook 'julia-shell-mode)
;;
;;   (define-key julia-mode-map (kbd "C-c C-c") 'julia-shell-run-region-or-line)
;;   (define-key julia-mode-map (kbd "C-c C-s") 'julia-shell-save-and-go)
;;   )


(provide 'init-my-prog-lang-julia)

;;; init-my-prog-lang-julia.el ends here
