;;; init-my-prog-vcs.el --- init Version Control System for Programming
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'my-prog-vcs-map)
  (define-prefix-command 'my-prog-vcs-map))
(global-set-key (kbd "C-c v") 'my-prog-vcs-map)


;;; [ vc-mode ] ---

;;; Usage:
;; - [C-x v] -- prefix. `vc-*'.
;; - [C-x v C-h] -- get keybinding with its prefix.




(require 'init-my-prog-vcs-git)
(require 'init-my-prog-vcs-diff)


(provide 'init-my-prog-vcs)

;;; init-my-prog-vcs.el ends here
