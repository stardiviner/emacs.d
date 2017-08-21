;;; init-my-prog-lang-lua.el --- init Lua
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ lua-mode ]

(use-package lua-mode
  :ensure t
  :bind (:map lua-mode-map
              ("C-c C-s" . run-lua))
  :config
  (setq lua-indent-level 3
        lua-always-show t
        lua-documentation-function 'browse-url
        lua-prefix-key (kbd "C-c")
        lua-indent-string-contents t
        lua-jump-on-traceback t
        )
  )


;;; [ ob-lua ]

(require 'ob-lua)

(add-to-list 'org-babel-load-languages '(lua . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("lua" . "lua"))


;;; [ company-lua ]

(use-package company-lua
  :ensure t
  :init
  (add-hook 'lua-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'company-lua)
              ))
  )


(provide 'init-my-prog-lang-lua)

;;; init-my-prog-lang-lua.el ends here
