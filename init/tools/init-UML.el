;;; init-UML.el --- init for UML tools.

;;; Commentary:



;;; Code:

;;; [ plantuml-mode ] -- Major mode for PlantUML.

(use-package plantuml-mode
  :ensure t
  :defer t
  :init
  (setq plantuml-jar-path (locate-user-emacs-file "init/extra/plantuml.jar"))
  )

;; [ ob-plantuml ]
(require 'ob-plantuml)
(setq org-plantuml-jar-path plantuml-jar-path)
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(add-to-list 'org-babel-load-languages '(clojure . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images 'append)



(provide 'init-UML)

;;; init-UML.el ends here
