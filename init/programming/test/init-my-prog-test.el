;;; init-my-prog-test.el --- init for Programming Test
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'my-prog-test-map)
  (define-prefix-command 'my-prog-test-map))
(global-set-key (kbd "C-c t") 'my-prog-test-map)


;;; [ cerbere ] -- Unit testing in Emacs for several programming languages



;;; [ test-case-mode ] -- unit test front-end



(provide 'init-my-prog-test)

;;; init-my-prog-test.el ends here
