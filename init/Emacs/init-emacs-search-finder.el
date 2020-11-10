;;; init-emacs-search-finder.el --- init for command find
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'find-prefix)
  (define-prefix-command 'find-prefix))

(define-key search-prefix (kbd "f") 'find-prefix)

;; (use-package helm
;;   :ensure t
;;   :bind (:map find-prefix ("h" . helm-find)))

(use-package helm-fd
  :ensure t
  :defer t
  :bind (:map find-prefix ("h" . helm-fd)))

;;; [ counsel-fd ] -- counsel interface for fd.

(use-package counsel-fd
  :ensure t
  :defer t
  :commands (counsel-fd-file-jump counsel-fd-dired-jump)
  :bind (:map find-prefix
              ("f" . counsel-fd-file-jump)
              ("d" . counsel-fd-dired-jump)))

;;; [ fd-dired ] -- find-dired alternative using fd.

(use-package fd-dired
  :ensure t
  :defer t
  :custom (fd-dired-display-in-current-window nil)
  :commands (fd-dired)
  :hook (fd-dired-display-in-current-window nil)
  :init (add-to-list 'display-buffer-alist '("^\\*Fd.*\\*" . (display-buffer-below-selected)))
  (defalias 'fd-search-async 'fd-dired)
  :bind (:map find-prefix ("s" . fd-search-async)))

;;; [ find-file-in-project ] -- Find file/directory and review Diff/Patch/Commit efficiently.

;; (use-package find-file-in-project
;;   :ensure t
;;   :defer t
;;   :commands (find-file-in-project find-file-in-project-by-selected)
;;   :bind ("M-t" . find-file-in-project) ; same with `counsel-git'
;;   :init (setq ffip-use-rust-fd t))


(provide 'init-emacs-search-finder)

;;; init-emacs-search-finder.el ends here
