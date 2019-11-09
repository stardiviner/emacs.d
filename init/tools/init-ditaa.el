;;; init-ditaa.el --- init for ditaa tools.

;;; Commentary:



;;; Code:

;;; [ ob-ditaa ]

(use-package ob-ditaa
  :defer t
  :commands (org-babel-execute:ditaa)
  :config
  ;; org-contrib-plus source code contrib/scripts/ contains this jar.
  ;; (setq org-ditaa-jar-path (expand-file-name "init/extra/ditaa0_9.jar" user-emacs-directory))
  (add-to-list 'org-babel-load-languages '(ditaa . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))



(provide 'init-ditaa)

;;; init-ditaa.el ends here
