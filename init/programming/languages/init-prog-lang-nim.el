;;; init-prog-lang-nim.el --- init for Nim language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ nim-mode ]

(use-package nim-mode
  :ensure t
  :ensure-system-package nim
  :config (add-hook 'nim-mode-hook 'nimsuggest-mode))


;;; [ nimrod-mode ]


;;; [ flycheck-nim ]

(use-package flycheck-nim
  :ensure t)


;;; [ nimsuggest ]


;;; [ ob-nim ] -- Babel Functions for Nim Language.

(use-package ob-nim
  :ensure t
  :defer t
  :commands (org-babel-execute:nim)
  :config
  (add-to-list 'org-babel-load-languages '(nim . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("nim" . "nim")))


(provide 'init-prog-lang-nim)

;;; init-prog-lang-nim.el ends here
