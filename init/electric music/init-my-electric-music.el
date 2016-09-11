;;; init-my-electric-music.el --- init for Electric Music
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ SuperCollider ] -- an audio server, programming language, and IDE for sound synthesis and algorithmic composition.

(add-to-list 'load-path "/usr/share/emacs/site-lisp/SuperCollider/")

(require 'sclang)

;;; Sclang Interface
(setq sclang-auto-scroll-post-buffer t
      sclang-show-workspace-on-startup nil
      sclang-use-symbol-table t
      sclang-main-run nil
      sclang-main-stop nil
      )

;;; Sclang mode
(setq sclang-indent-level 2)

;;; Sclang minor mode



(provide 'init-my-electric-music)

;;; init-my-electric-music.el ends here
