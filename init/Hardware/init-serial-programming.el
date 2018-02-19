;;; init-serial-programming.el --- init for Serial Programming.

;;; Commentary:



;;; Code:

;;; [ ob-uart ] -- A wrapper around `make-serial-process' to integrate of UART communication for Org-mode Babel.

(use-package ob-uart
  :ensure t
  :defer t
  :config
  (add-to-list 'org-babel-load-languages '(uart . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  )



(provide 'init-serial-programming)

;;; init-serial-programming.el ends here
