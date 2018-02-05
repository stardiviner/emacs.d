;;; init-ditaa.el --- init for ditaa tools.

;;; Commentary:



;;; Code:

;;; [ ob-ditaa ]

(require 'ob-ditaa)
;; org-contrib-plus source code contrib/scripts/ contains this jar.
;; (setq org-ditaa-jar-path "~/.emacs.d/init/extra/ditaa0_9.jar")

(add-to-list 'org-babel-load-languages '(ditaa . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)



(provide 'init-ditaa)

;;; init-ditaa.el ends here
