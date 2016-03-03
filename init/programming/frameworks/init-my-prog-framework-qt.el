;;; init-my-prog-framework-qt.el --- init for Qt
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Qt mode ]




;;; [ QML-mode ]

(use-package qml-mode
  :ensure t)


;;; [ company-qml ]

(use-package company-qml
  :ensure t
  :config
  (add-hook 'qml-mode-hook
            (lambda ()
              (setq-local company-minimum-prefix-length 0)
              (my-company-add-backends-to-mode '(company-qml))
              ))
  )


(provide 'init-my-prog-framework-qt)

;;; init-my-prog-framework-qt.el ends here
