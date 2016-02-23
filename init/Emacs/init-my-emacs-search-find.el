;;; init-my-emacs-search-find.el --- init for command find
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ find-file-in-project ]

(use-package find-file-in-project
  :ensure t
  :config
  (setq ffip-project-root "~")
  ;; (setq ffip-project-root-function)

  ;; (setq ffip-match-path-instead-of-filename t)
  
  (global-set-key (kbd "C-x f") 'find-file-in-project)
  )


(provide 'init-my-emacs-search-find)

;;; init-my-emacs-search-find.el ends here
