;;; init-make.el --- init for Make utility.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Make ]

;;; [ make-mode ]

(use-package make-mode
  :ensure t
  :defer t)


;;; [ ob-makefile ]

(use-package ob-makefile
  :defer t
  :commands (org-babel-execute:makefile)
  :config
  (add-to-list 'org-babel-load-languages '(makefile . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))


;;; [ makefile-executor ] -- Emacs helpers to run things from makefiles.

(use-package makefile-executor
  :ensure t
  :defer t
  :commands (makefile-executor-mode)
  :bind (:map build-system-prefix ("<f6>" . makefile-executor-execute-project-target))
  :init (add-hook 'makefile-mode-hook 'makefile-executor-mode))


(provide 'init-make)

;;; init-make.el ends here
