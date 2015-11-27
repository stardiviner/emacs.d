;;; init-my-prog-lang-dart.el --- init for Dart
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ dart-mode ]

(use-package dart-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))
  :config
  (setq dart-font-lock-extra-types t
        dart-enable-analysis-server t
        ;; dart-analysis-server-snapshot-path
        )
  )



(provide 'init-my-prog-lang-dart)

;;; init-my-prog-lang-dart.el ends here
