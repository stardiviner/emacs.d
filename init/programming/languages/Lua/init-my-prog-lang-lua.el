;;; init-my-prog-lang-lua.el --- init Lua
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ lua-mode ]

(use-package lua-mode
  :ensure t
  :config
  (setq lua-indent-level 3
        lua-always-show t
        lua-documentation-function 'browse-url
        lua-prefix-key (kbd "C-c")
        lua-indent-string-contents t
        lua-jump-on-traceback t
        )

  (define-key lua-mode-map (kbd "C-c C-s") 'run-lua)
  )


;;; [ company-lua ]

(use-package company-lua
  :ensure t
  :config
  (add-hook 'lua-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'company-lua)
              ))
  )


(provide 'init-my-prog-lang-lua)

;;; init-my-prog-lang-lua.el ends here
