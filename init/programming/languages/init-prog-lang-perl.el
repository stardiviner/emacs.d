;;; init-prog-lang-perl.el --- init for Perl
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ perl-mode ]

(use-package perl-mode
  :ensure t
  :defer t)

;;; [ perl6-mode ]

(use-package perl6-mode
  :ensure t
  :defer t)

;;; [ ob-perl ]

(use-package ob-perl
  :defer t
  :commands (org-babel-execute:perl)
  :config
  (add-to-list 'org-babel-load-languages '(perl . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;;; [ plsense ] -- interface for PlSense that is a development tool for Perl.

;; (use-package plsense
;;   :ensure t
;;   :defer t
;;   :init
;;   (setq plsense-popup-help-key "M-h"
;;         plsense-display-help-buffer-key "C-h"
;;         plsense-jump-to-definition-key "M-.")
;;
;;   (plsense-config-default)
;;
;;   (use-package company-plsense
;;     :ensure t
;;     :init
;;     (dolist (hook '(perl-mode-hook
;;                     cperl-mode-hook))
;;       (add-hook hook
;;                 (lambda () (my-company-add-backend-locally 'company-plsense))))
;;     )
;;   )


(provide 'init-prog-lang-perl)

;;; init-prog-lang-perl.el ends here
