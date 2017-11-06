;;; init-my-prog-lang-erlang.el --- init for Erlang
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ erlang-mode ]

(use-package erlang
  :ensure t)

;;; [ company-erlang ] -- company backend based on ivy-erlang-complete.

(use-package company-erlang
  :ensure t
  :defer t
  :init
  (add-hook 'erlang-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'company-erlang)))
  )

;;; [ edts ] -- Erlang Development Tool Suite

;; (use-package edts
;;   :ensure t
;;   :defer t)


;;; [ distel ] -- Erlang IDE (Distributed Emacs Lisp for Erlang)

(use-package distel
  :load-path "/usr/share/distel/elisp/"
  :defer t
  :init
  ;; (distel-setup)
  (add-hook 'erlang-mode-hook 'distel-erlang-mode-hook)
  :config
  ;; [ distel-completion-lib ] -- Erlang/distel completion backend for both auto-complete and company-mode.
  (use-package distel-completion-lib
    :ensure t)
  ;; [ company-distel ] -- Erlang/distel completion backend for company-mode.
  (use-package company-distel
    :ensure t
    :init
    (add-hook 'erlang-mode-hook
              (lambda ()
                (my-company-add-backend-locally 'company-distel)))
    :config
    (setq company-distel-popup-help t)
    )
  )


(provide 'init-my-prog-lang-erlang)

;;; init-my-prog-lang-erlang.el ends here
