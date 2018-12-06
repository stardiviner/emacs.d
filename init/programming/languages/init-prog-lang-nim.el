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
  :ensure t)


(provide 'init-prog-lang-nim)

;;; init-prog-lang-nim.el ends here
