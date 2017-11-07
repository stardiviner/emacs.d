;;; init-my-org-presentation.el --- init for Org presentation
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ org-tree-slide ] -- A presentation tool for org-mode based on the visibility of outline trees.

(use-package org-tree-slide
  :ensure t
  :config
  (setq org-tree-slide-header t
        org-tree-slide-cursor-init t
        org-tree-slide-skip-done nil
        org-tree-slide-slide-in-effect t
        org-tree-slide-heading-emphasis t
        org-tree-slide-modeline-display 'outside
        org-tree-slide-fold-subtrees-skipped t)
  
  ;; profiles
  ;; (org-tree-slide-simple-profile)
  (org-tree-slide-presentation-profile)
  ;; (org-tree-slide-narrowing-control-profile)

  (define-key my-org-prefix (kbd "C-s") 'org-tree-slide-mode)
  ;; (global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
  
  (define-key org-tree-slide-mode-map (kbd "C-SPC") 'org-tree-slide-move-next-tree)
  ;; (define-key org-tree-slide-mode-map (kbd "SPC") 'org-tree-slide-move-next-tree)
  (define-key org-tree-slide-mode-map (kbd "S-SPC") 'org-tree-slide-move-previous-tree)
  )


;;; [ ox-reveal ] -- Org-mode export with Reveal.js.

(use-package ox-reveal
  :ensure t
  :init
  (use-package htmlize
    :ensure t)
  :config
  ;; "http://cdn.jsdelivr.net/reveal.js/3.0.0/"
  ;; (concat user-emacs-directory "init/org-mode/reveal.js")
  (setq org-reveal-root (expand-file-name (concat user-emacs-directory "Org-mode/reveal.js/")))
  (setq org-reveal-plugins '(classList markdown zoom notes
                                       highlight search remotes multiplex))
  (setq org-reveal-highlight-css "%r/lib/css/solarized.css")
  (setq org-reveal-default-frag-style t)
  )


;;; [ ox-ioslide ] -- Export org-mode to Google I/O HTML5 slide.

;; (use-package makey
;;   :ensure t
;;   )
;;
;; (use-package ox-ioslide
;;   :ensure t
;;   :config
;;   (require 'ox-ioslide-helper)
;;   )

;;; [ demo-it ] -- demonstrations and presentations with `org-tree-slide' from within Emacs.

(use-package demo-it
  :ensure t)


(provide 'init-my-org-presentation)

;;; init-my-org-presentation.el ends here
