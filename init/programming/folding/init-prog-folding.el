;;; init-prog-folding.el --- init for Programming Folding
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'prog-fold-prefix)
  (define-prefix-command 'prog-fold-prefix))
(global-set-key (kbd "C-c SPC") 'prog-fold-prefix)

;;; [ hideshow ] -- minor mode to selectively hide/show code and comment blocks.

;; (use-package hideshow
;;   :init
;;   (use-package hideshowvis ; Add markers to the fringe for regions foldable by hideshow.
;;     :ensure t
;;     :init (add-hook 'prog-mode-hook #'hideshowvis-enable)
;;     :config (hideshowvis-symbols)))

;;; [ origami ] -- A folding minor mode for Emacs.

(use-package origami
  :ensure t
  :bind (:map prog-fold-prefix
              ("m"  . origami-mode)
              ("SPC" .  origami-toggle-node)
              ("TAB". origami-toggle-all-nodes)
              ("n" . origami-next-fold)
              ("p" . origami-previous-fold)
              ("c" . origami-close-node)
              ("C" . origami-close-all-nodes)
              ("o" . origami-open-node)
              ("O" . origami-open-all-nodes)
              ("T" . origami-recursively-toggle-node)
              (">" . origami-open-node-recursively)
              ("<" . origami-close-node-recursively)
              ("O" . origami-show-only-node)
              ("u" . origami-undo)
              ("r" . origami-redo)
              ("!" . origami-reset)
              )
  :config
  (setq origami-show-fold-header t
        origami-fold-replacement "...")

  ;; `global-origami-mode' & `origami-mode'
  (dolist (hook '(prog-mode-hook
                  ))
    (add-hook hook 'origami-mode))
  )


(provide 'init-prog-folding)

;;; init-prog-folding.el ends here
