;;; init-my-prog-folding.el --- init for Programming Folding
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'my-fold-map)
  (define-prefix-command 'my-fold-map))
(global-set-key (kbd "C-c SPC") 'my-fold-map)


;;; [ hs-minor-mode ] -- hide/show

;; (add-hook 'prog-mode-hook 'hs-minor-mode)


;;; [ origami ] -- A folding minor mode for Emacs.

(use-package origami
  :ensure t
  :config
  (setq origami-show-fold-header t
        origami-fold-replacement "...")

  (set-face-attribute 'origami-fold-header-face nil
                      :inherit nil
                      :foreground nil :background "#004A5D"
                      :box '(:color "#40CBCB" :line-width 1)
                      )
  (set-face-attribute 'origami-fold-fringe-face nil
                      :inherit nil
                      :foreground "black" :background "#40CBCB"
                      )
  (set-face-attribute 'origami-fold-replacement-face nil
                      :inherit 'origami-fold-header-face
                      :foreground "cyan" :background nil
                      )
  
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

  ;; `global-origami-mode' & `origami-mode'
  (dolist (hook '(prog-mode-hook
                  ))
    (add-hook hook 'origami-mode))
  )


(provide 'init-my-prog-folding)

;;; init-my-prog-folding.el ends here
