;;; init-ditaa.el --- init for ditaa tools.

;;; Commentary:



;;; Code:

;;; [ ob-ditaa ]

(require 'ob-ditaa)
;; (setq org-ditaa-jar-path "~/.emacs.d/init/extra/ditaa0_9.jar") ; Org-mode source code contrib/scripts/ contains this jar.

(add-to-list 'org-babel-load-languages '(ditaa . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)



(provide 'init-ditaa)

;;; init-ditaa.el ends here
