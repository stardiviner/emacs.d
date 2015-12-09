;;; init-my-prog-lang-nim.el --- init for Nim language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ nim-mode ]

(use-package nim-mode
  :config
  (set-face-attribute 'nim-tab-face nil
                      :foreground "dark gray"
                      :background "grey22")

  ;; company-mode support
  (require 'company-nim)
  (add-hook 'nim-mode-hook
            (lambda ()
              (my-company-add-backends-to-mode '(company-nim))))
  )


;;; [ flycheck-nim ]





(provide 'init-my-prog-lang-nim)

;;; init-my-prog-lang-nim.el ends here
