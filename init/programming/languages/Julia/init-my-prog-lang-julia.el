;;; init-my-prog-lang-julia.el --- init for Julia
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ julia-mode ]

(use-package julia-mode
  :ensure t
  :defer t
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
  (unless (boundp 'my-inferior-ess-map)
    (define-prefix-command 'my-inferior-ess-map))
  (define-key my-inferior-ess-map (kbd "j") 'my-ess-inferior-julia)
  
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

(use-package org-plus-contrib
  :ensure t
  :config
  (require 'ob-julia)

  (if (not (boundp 'inferior-julia-program-name))
      (setq inferior-julia-program-name "julia"))
  ;; (setq org-babel-julia-command "julia")

  (add-to-list 'org-babel-load-languages '(julia . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  
  (use-package ess
    :ensure t
    :config
    (require 'ess-site))

  (setq org-babel-default-header-args:julia
        '((:results . "output replace")
          (:padnewline . "yes")))
  (add-to-list 'org-src-lang-modes '("julia" . ess-julia))
  )



(provide 'init-my-prog-lang-julia)

;;; init-my-prog-lang-julia.el ends here
