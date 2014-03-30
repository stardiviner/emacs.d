;;; init-my-emacs-search.el --- init search utilities for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; keybindings prefix

(define-prefix-command 'my-search-prefix-map)
(global-set-key (kbd "C-c s") 'my-search-prefix-map)




(provide 'init-my-emacs-search)

;;; init-my-emacs-search.el ends here
