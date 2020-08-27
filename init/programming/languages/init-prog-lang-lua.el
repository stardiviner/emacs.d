;;; init-prog-lang-lua.el --- init Lua
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ lua-mode ]

(use-package lua-mode
  :ensure t
  :defer t
  :commands (run-lua)
  :bind (:map lua-mode-map ("C-c C-s" . run-lua))
  :custom ((lua-documentation-function 'eww)
           ;; (lua-indent-string-contents t)
           )
  :init (add-to-list 'display-buffer-alist '("^\\*lua\\*" . (display-buffer-below-selected)))
  :config
  ;; Fix Lua indentation lisp lisp-y error
  (defun lua-busted-fuckups-fix ()
    (save-excursion
      (lua-forward-line-skip-blanks 'back)
      (let* ((current-indentation (current-indentation))
             (line (thing-at-point 'line t))
             (busted-p (s-matches?
			                  (rx (+ bol (* space)
                               (or "context" "describe" "it" "setup" "teardown")
                               "("))
			                  line)))
        (when busted-p
          (+ current-indentation lua-indent-level)))))

  (defun rgc-lua-calculate-indentation-override (old-function &rest arguments)
    (or (lua-busted-fuckups-fix)
	      (apply old-function arguments)))

  (advice-add #'lua-calculate-indentation-override
	            :around #'rgc-lua-calculate-indentation-override))


;;; [ ob-lua ]

(use-package ob-lua
  :defer t
  :commands (org-babel-execute:lua)
  :config
  (add-to-list 'org-babel-load-languages '(lua . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("lua" . "lua")))


;;; [ company-lua ]

(use-package company-lua
  :ensure t
  :defer t
  :after lua-mode
  :init
  (defun my/company-lua-setup ()
    (my-company-add-backend-locally 'company-lua))
  (add-hook 'lua-mode-hook #'my/company-lua-setup))


(provide 'init-prog-lang-lua)

;;; init-prog-lang-lua.el ends here
