;;; init-my-prog-lang-lua.el --- init Lua
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ lua-mode ]

(use-package lua-mode
  :ensure t
  :ensure-system-package lua
  :defer t
  :bind (:map lua-mode-map ("C-c C-s" . run-lua))
  :config
  (setq lua-documentation-function 'eww
        ;; lua-indent-string-contents t
        )
  (add-to-list 'display-buffer-alist
               '("^\\*lua\\*" (display-buffer-below-selected)))
  )


;;; [ ob-lua ]

(require 'ob-lua)

(add-to-list 'org-babel-load-languages '(lua . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("lua" . "lua"))


;;; [ company-lua ]

(use-package company-lua
  :ensure t
  :defer t
  :after lua-mode
  :init
  (add-hook 'lua-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'company-lua)
              ))
  )


(provide 'init-my-prog-lang-lua)

;;; init-my-prog-lang-lua.el ends here
