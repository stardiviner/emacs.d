;;; init-my-prog-lang-nim.el --- init for Nim language
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ nim-mode ]

(use-package nim-mode
  :config
  (setq nim-args-compile '()
        ;; nim-fill-docstring-style
        )

  (set-face-attribute 'nim-tab-face nil
                      :foreground "dark gray"
                      :background "grey22")
  )


(provide 'init-my-prog-lang-nim)

;;; init-my-prog-lang-nim.el ends here
