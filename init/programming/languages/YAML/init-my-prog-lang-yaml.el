;;; init-my-prog-lang-yaml.el --- init for YAML
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ yaml-mode ]

(use-package yaml-mode
  :ensure t
  :defer t
  :mode (("\\.yaml$" . yaml-mode)
         ("\\.yml$" . yaml-mode))
  :config
  (setq yaml-indent-offset 4)
  )


(provide 'init-my-prog-lang-yaml)

;;; init-my-prog-lang-yaml.el ends here
