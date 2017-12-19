;;; init-UML.el --- init for UML tools.

;;; Commentary:



;;; Code:

;;; [ plantuml-mode ] -- Major mode for PlantUML.

(use-package plantuml-mode
  :ensure t
  :defer t
  :config
  (setq plantuml-jar-path (locate-user-emacs-file "init/extra/plantuml.jar"))

  
  ;; [ ob-plantuml ]
  (require 'ob-plantuml)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml-mode))
  (setq org-plantuml-jar-path plantuml-jar-path)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images 'append)
  )



(provide 'init-UML)

;;; init-UML.el ends here
