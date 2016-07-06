;;; init-spice.el --- init for SPICE
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ spice-mode ] -- major mode for SPICE.

(autoload 'spice-mode "spice-mode" "Spice  Editing Mode" t)
(setq auto-mode-alist
      (append '(("\\.sp$"  . spice-mode)) auto-mode-alist))


;;; [ ob-spice ] -- Org-mode Babel integrate with SPICE.

(require 'ob-spice)


(provide 'init-spice)

;;; init-spice.el ends here
