;;; init-prog-lang-dart.el --- init for Dart
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ dart-mode ]

(use-package dart-mode
  :ensure t
  :config (setq dart-font-lock-extra-types t
                dart-enable-analysis-server t))

;;; [ company-dart ]

(use-package company-dart
  :ensure t
  :init
  (defun my/company-dart-setup ()
    (my-company-add-backend-locally 'company-dart))
  (add-hook 'dart-mode-hook #'my/company-dart-setup))


(provide 'init-prog-lang-dart)

;;; init-prog-lang-dart.el ends here
