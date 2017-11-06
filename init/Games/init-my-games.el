;;; init-my-games.el --- init for Games
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(use-package gnugo
  :ensure t
  :defer t)

;;; [ go ] -- The el-go library provides an Emacs interface to the game of GO.

(use-package go
  :ensure t
  :defer t)

;;; [ sudoku ] -- simple sudoku game, can download puzzles.

(use-package sudoku
  :ensure t)


(provide 'init-my-games)

;;; init-my-games.el ends here
