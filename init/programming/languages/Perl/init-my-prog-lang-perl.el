;;; init-my-prog-lang-perl.el --- init for Perl
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ perl-mode ]

(use-package perl-mode
  :ensure t)

;;; [ perl6-mode ]

(use-package perl6-mode
  :ensure t)

;;; [ plsense ] -- interface for PlSense that is a development tool for Perl.

(use-package plsense
  :ensure t
  :config
  (setq plsense-popup-help-key "M-h"
        plsense-display-help-buffer-key "C-h"
        plsense-jump-to-definition-key "M-.")
  
  (plsense-config-default)
  )


(provide 'init-my-prog-lang-perl)

;;; init-my-prog-lang-perl.el ends here
