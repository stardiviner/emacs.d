;;; init-my-prog-eval.el --- init for interactive evaluation.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'my-prog-eval-map)
  (define-prefix-command 'my-prog-eval-map))
(global-set-key (kbd "C-c e") 'my-prog-eval-map)


(provide 'init-my-prog-eval)

;;; init-my-prog-eval.el ends here
