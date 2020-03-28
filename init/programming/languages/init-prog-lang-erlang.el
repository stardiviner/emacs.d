;;; init-prog-lang-erlang.el --- init for Erlang
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ erlang-mode ]

(use-package erlang
  :ensure t
  :defer t
  :commands (run-erlang))

;;; [ company-erlang ] -- company backend based on ivy-erlang-complete.

(use-package company-erlang
  :ensure t
  :defer t
  :init
  (defun my/company-erlang-setup ()
    (my-company-add-backend-locally 'company-erlang))
  (add-hook 'erlang-mode-hook #'my/company-erlang-setup))

;;; [ edts ] -- Erlang Development Tool Suite

;; (use-package edts
;;   :ensure t
;;   :defer t)


;;; [ distel ] -- Erlang IDE (Distributed Emacs Lisp for Erlang)

;; (use-package distel-completion-lib
;;   :load-path "/usr/share/distel/elisp/"
;;   :defer t
;;   :init
;;   (add-hook 'erlang-mode-hook 'distel-erlang-mode-hook)
;;   :config
;;   ;; (distel-setup)
;;   )

;; [ distel-completion-lib ] -- Erlang/distel completion backend for both auto-complete and company-mode.

(use-package distel-completion-lib
  :ensure t
  :init
  ;; [ company-distel ] -- Erlang/distel completion backend for company-mode.
  (use-package company-distel
    :ensure t
    :init
    (defun my/company-distel-setup ()
      (my-company-add-backend-locally 'company-distel))
    (add-hook 'erlang-mode-hook #'my/company-distel-setup)
    :config (setq company-distel-popup-help t)))


(provide 'init-prog-lang-erlang)

;;; init-prog-lang-erlang.el ends here
