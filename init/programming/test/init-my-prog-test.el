;;; init-my-prog-test.el --- init for Programming Test
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'prog-test-prefix)
  (define-prefix-command 'prog-test-prefix))
(global-set-key (kbd "C-c t") 'prog-test-prefix)


;;; [ cerbere ] -- Unit testing in Emacs for several programming languages



;;; [ test-case-mode ] -- unit test front-end



(provide 'init-my-prog-test)

;;; init-my-prog-test.el ends here
