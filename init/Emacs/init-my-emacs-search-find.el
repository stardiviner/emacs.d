;;; init-my-emacs-search-find.el --- init for command find
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ find-file-in-project ]

(use-package find-file-in-project
  :ensure t
  :config
  (setq ffip-project-root nil)
  (setq ffip-project-root-function ffip-project-root)
  (defun ffip-project-root ()
    "Return the root of the project."
    (let ((project-root (or ffip-project-root
                            (if (functionp ffip-project-root-function)
                                (funcall ffip-project-root-function)
                              (if (listp ffip-project-file)
                                  (cl-some (apply-partially 'locate-dominating-file
                                                            default-directory)
                                           ffip-project-file)
                                (locate-dominating-file default-directory
                                                        ffip-project-file))))))
      (or project-root
          (progn (message "No project was defined for the current file.")
                 "."
                 ;; nil
                 ))))
  
  ;; default project root
  ;; (setq ffip-project-root "~") ; home directory
  ;; (setq ffip-project-root ".") ; current directory

  ;; (setq ffip-match-path-instead-of-filename t)
  
  (global-set-key (kbd "C-x f") 'find-file-in-project)
  )


(provide 'init-my-emacs-search-find)

;;; init-my-emacs-search-find.el ends here
