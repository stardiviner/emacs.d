;;; init-my-prog-lang-nim.el --- init for Nim language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ nim-mode ]

(use-package nim-mode
  :ensure t
  :config
  (set-face-attribute 'nim-tab-face nil
                      :foreground "dark gray"
                      :background "grey22")

  ;; company-mode support
  (require 'company-nim)
  (add-hook 'nim-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends) 'company-nim)))
  )


;;; [ nimrod-mode ]


;;; [ flycheck-nim ]

(use-package flycheck-nim
  :ensure t)


;;; [ nimsuggest ]




(provide 'init-my-prog-lang-nim)

;;; init-my-prog-lang-nim.el ends here
