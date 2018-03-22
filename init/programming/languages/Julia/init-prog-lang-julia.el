;;; init-prog-lang-julia.el --- init for Julia
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ julia-mode ]

(use-package julia-mode
  :ensure t
  :ensure-system-package julia
  :defer t
  :commands (run-julia)
  :config
  (setq julia-indent-offset 2)
  )


;;; [ ESS - Julia ]

(use-package ess
  :ensure t
  :defer t
  :init
  (add-hook 'julia-mode-hook
            (lambda ()
              ;; add julia-mode to prog-mode.
              (unless (derived-mode-p 'prog-mode)
                (run-hooks 'prog-mode-hook))
              ))
  
  ;; - `julia' :: from ess-julia.
  ;; - `inferior-julia' :: from julia-mode.
  (unless (boundp 'ess-prefix)
    (define-prefix-command 'ess-prefix))
  (define-key ess-prefix (kbd "j") 'my-ess-inferior-julia)
  
  :config
  ;; (setq inferior-julia-args)

  (defun my-ess-inferior-julia (&optional process-buffer-name)
    "Start or switch to inferior-julia process buffer PROCESS-BUFFER-NAME."
    (interactive)
    (if (get-buffer-process (or process-buffer-name "*julia*"))
        ;; the inferior julia process exist
        (switch-to-buffer (or process-buffer-name "*julia*"))
      ;; create a new inferior julia process
      (julia)
      ;; (julia)
      ;; kill old process
      ;; (kill-process (get-buffer-process (or process-buffer-name "*julia*"))
      )
    )
  )

;;; [ ob-julia ]

(use-package ess
  :ensure t
  :load (ess-site ess-custom)
  :init
  (if (boundp 'inferior-julia-program-name)
      (setq inferior-julia-program-name "julia")))

(require 'ob-julia)

(setq org-babel-julia-command "julia")

(add-to-list 'org-babel-load-languages '(julia . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("julia" . "jl"))

(setq org-babel-default-header-args:julia
      '((:results . "output replace")
        (:padnewline . "yes")))
(add-to-list 'org-src-lang-modes '("julia" . ess-julia))

;;; [ julia-shell ] -- Emacs major mode for an interactive Julia shell.

;; (use-package julia-shell
;;   :ensure t
;;   :commands (run-julia))

;;; [ flycheck-julia ] -- Add a julia syntax checker to flycheck using Lint.jl

;; (use-package flycheck-julia
;;   :ensure t
;;   :defer t
;;   :config
;;   (flycheck-julia-setup)
;;   ;; if you use `flycheck-global-mode'
;;   ;; (add-to-list 'flycheck-global-modes 'julia-mode)
;;   ;; (add-to-list 'flycheck-global-modes 'ess-julia-mode)
;;   )


(provide 'init-prog-lang-julia)

;;; init-prog-lang-julia.el ends here
