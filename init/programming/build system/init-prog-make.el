;;; init-prog-make.el --- init for Make utility.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Make ]

;;; [ make-mode ]

(use-package make-mode
  :ensure t
  :defer t)


;;; [ ob-makefile ]

(require 'ob-makefile)

(add-to-list 'org-babel-load-languages '(makefile . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
;; FIXME: (add-to-list 'org-babel-tangle-lang-exts '("makefile" . "Makefile"))


;;; [ makefile-executor ] -- Emacs helpers to run things from makefiles.

(use-package makefile-executor
  :ensure t
  :defer t
  :bind (:map build-system-prefix
              ("<f5>" . makefile-executor-execute-project-target))
  :init
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))


(provide 'init-prog-make)

;;; init-prog-make.el ends here
