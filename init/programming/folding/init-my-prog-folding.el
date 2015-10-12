;;; init-my-prog-folding.el --- init for Programming Folding
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ origami ]

;;; `global-origami-mode' & `origami-mode'

(use-package origami
  :config
  (define-key my-outline-prefix (kbd "m") 'origami-mode)
  (define-key my-outline-prefix (kbd "SPC") 'origami-toggle-node)
  (define-key my-outline-prefix (kbd "TAB") 'origami-toggle-all-nodes)
  (define-key my-outline-prefix (kbd "n") 'origami-next-fold)
  (define-key my-outline-prefix (kbd "p") 'origami-previous-fold)
  (define-key my-outline-prefix (kbd "c") 'origami-close-node)
  (define-key my-outline-prefix (kbd "C") 'origami-close-all-nodes)
  (define-key my-outline-prefix (kbd "o") 'origami-open-node)
  (define-key my-outline-prefix (kbd "O") 'origami-open-all-nodes)
  (define-key my-outline-prefix (kbd "T") 'origami-recursively-toggle-node)
  (define-key my-outline-prefix (kbd ">") 'origami-open-node-recursively)
  (define-key my-outline-prefix (kbd "<") 'origami-close-node-recursively)
  (define-key my-outline-prefix (kbd "O") 'origami-show-only-node)
  (define-key my-outline-prefix (kbd "u") 'origami-undo)
  (define-key my-outline-prefix (kbd "r") 'origami-redo)
  (define-key my-outline-prefix (kbd "!") 'origami-reset)
  )



(provide 'init-my-prog-folding)

;;; init-my-prog-folding.el ends here
