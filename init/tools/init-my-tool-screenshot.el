;;; init-my-tool-screenshot.el --- init for Screenshot
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'screenshot-prefix)
  (define-prefix-command 'screenshot-prefix))
(define-key tools-prefix (kbd "S") 'screenshot-prefix)

(use-package frameshot
  :ensure t
  :commands (frameshot-mode frameshot-take)
  :bind (:map screenshot-prefix ("S" . frameshot-mode)))


(provide 'init-my-tool-screenshot)

;;; init-my-tool-screenshot.el ends here
