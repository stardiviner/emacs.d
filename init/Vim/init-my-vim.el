;;; init-my-vim.el --- init for Vim
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Evil ]

(use-package evil
  :ensure t)


;;; [ vimrc-mode ] -- major mode for vimrc files.

(use-package vimrc-mode
  :ensure t
  :defer t)


(provide 'init-my-vim)

;;; init-my-vim.el ends here
