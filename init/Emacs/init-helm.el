;;; init-helm.el --- simple configure Helm
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Helm ] -- 

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ;; ([remap org-goto] . helm-org-in-buffer-headings) ; [C-c C-j] completion for Org headings
         )
  :load (helm helm-config)
  :config
  (setq helm-split-window-inside-p t)

  ;; enable auto resize helm buffer
  (helm-autoresize-mode t)
  (if (null helm-autoresize-mode)
      (setq helm-display-buffer-default-height 10)
    (setq helm-autoresize-max-height 20
          helm-autoresize-min-height 10))

  (helm-mode 1)

  ;; Helm internal keybindings `helm-map'
  )


;; (use-package helm-git
;;   :ensure t
;;   :bind ("M-t" . helm-git))


(provide 'init-helm)

;;; init-helm.el ends here
