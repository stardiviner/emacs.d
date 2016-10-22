;;; init-my-prog-lang-erlang.el --- init for Erlang
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ erlang-mode ]

(use-package erlang
  :ensure t
  :defer t)


;;; [ edts ] -- Erlang Development Tool Suite

(use-package edts
  :ensure t
  :defer t)


;;; [ distel ]

(add-to-list 'load-path "/usr/share/distel/elisp/")


;;; [ distel-completion-lib ] -- Completion library for Erlang/Distel.

(use-package distel-completion-lib
  :ensure t
  :defer t
  :config
  ;; (setq distel-completion-get-doc-from-internet t)
  ;; (setq distel-completion-valid-syntax "a-zA-Z:_-")
  )


;;; [ company-distel ] -- Erlang/distel completion backend for company-mode.

(use-package company-distel
  :ensure t
  :defer t
  :init
  (add-hook 'erlang-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'company-distel)))
  
  :config
  (setq company-distel-popup-help t)
  )


(provide 'init-my-prog-lang-erlang)

;;; init-my-prog-lang-erlang.el ends here
