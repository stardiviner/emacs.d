;;; init-my-prog-folding.el --- init for Programming Folding
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'prog-fold-prefix)
  (define-prefix-command 'prog-fold-prefix))
(global-set-key (kbd "C-c SPC") 'prog-fold-prefix)

;;; [ hideshow ] -- minor mode to selectively hide/show code and comment blocks.

;; (use-package hideshow
;;   :ensure t
;;   :config
;;   ;; Add markers to the fringe for regions foldable by hideshow.
;;   (use-package hideshowvis
;;     :ensure t
;;     :config
;;     (add-hook 'prog-mode-hook #'hideshowvis-enable)
;;     (hideshowvis-symbols)
;;     )
;;   )

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
  :init
  ;; `global-origami-mode' & `origami-mode'
  (dolist (hook '(prog-mode-hook
                  ))
    (add-hook hook 'origami-mode))
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
  )


(provide 'init-my-prog-folding)

;;; init-my-prog-folding.el ends here
