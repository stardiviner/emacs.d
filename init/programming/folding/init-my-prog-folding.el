;;; init-my-prog-folding.el --- init for Programming Folding
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'my-fold-map)
  (define-prefix-command 'my-fold-map))
(global-set-key (kbd "C-c SPC") 'my-fold-map)


;;; [ hs-minor-mode ] -- hide/show

;; FIXME: *ERROR*: Web Mode doesn't support Hideshow Minor Mode.
;; (add-hook 'prog-mode-hook 'hs-minor-mode)


;;; [ origami ]

;;; `global-origami-mode' & `origami-mode'

(use-package origami
  :ensure t
  :config
  (define-key my-fold-map (kbd "m") 'origami-mode)
  (define-key my-fold-map (kbd "SPC") 'origami-toggle-node)
  (define-key my-fold-map (kbd "TAB") 'origami-toggle-all-nodes)
  (define-key my-fold-map (kbd "n") 'origami-next-fold)
  (define-key my-fold-map (kbd "p") 'origami-previous-fold)
  (define-key my-fold-map (kbd "c") 'origami-close-node)
  (define-key my-fold-map (kbd "C") 'origami-close-all-nodes)
  (define-key my-fold-map (kbd "o") 'origami-open-node)
  (define-key my-fold-map (kbd "O") 'origami-open-all-nodes)
  (define-key my-fold-map (kbd "T") 'origami-recursively-toggle-node)
  (define-key my-fold-map (kbd ">") 'origami-open-node-recursively)
  (define-key my-fold-map (kbd "<") 'origami-close-node-recursively)
  (define-key my-fold-map (kbd "O") 'origami-show-only-node)
  (define-key my-fold-map (kbd "u") 'origami-undo)
  (define-key my-fold-map (kbd "r") 'origami-redo)
  (define-key my-fold-map (kbd "!") 'origami-reset)

  (dolist (hook '(prog-mode-hook
                  ))
    (add-hook hook 'origami-mode))
  )


(provide 'init-my-prog-folding)

;;; init-my-prog-folding.el ends here
