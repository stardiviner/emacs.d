;;; init-spice.el --- init for SPICE
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ spice-mode ] -- major mode for SPICE.

(use-package spice-mode
  :ensure t
  :defer t)

;;; [ ob-spice ] -- Org-mode Babel integrate with SPICE.

(require 'ob-spice)
(add-to-list 'org-babel-load-languages '(spice . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)


(provide 'init-spice)

;;; init-spice.el ends here
