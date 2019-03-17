;;; init-prog-lang-applescript.el --- init for AppleScript
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ applescript-mode ]

(use-package applescript-mode
  :ensure t
  :defer t)

;;; [ apples-mode ]

(use-package apples-mode
  :ensure t
  :defer t)

;;; [ ob-applescript ]

(use-package ob-applescript
  :ensure t
  :defer t
  :commands (org-babel-execute:applescript)
  :config
  (add-to-list 'org-babel-load-languages '(applescript . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("applescript" . "applescript")))


(provide 'init-prog-lang-applescript)

;;; init-prog-lang-applescript.el ends here
