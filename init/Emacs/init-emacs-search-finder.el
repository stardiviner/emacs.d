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

;;; [ fd-dired ] -- find-dired alternative using fd.

(use-package fd-dired
  :ensure t
  :defer t
  :commands (fd-dired)
  :bind (:map find-prefix ("f" . fd-dired))
  :init (setq fd-dired-display-in-current-window nil))

;;; [ find-file-in-project ] -- Find file/directory and review Diff/Patch/Commit efficiently.

;; (use-package find-file-in-project
;;   :ensure t
;;   :defer t
;;   :commands (find-file-in-project find-file-in-project-by-selected)
;;   :bind ("M-t" . find-file-in-project) ; same with `counsel-git'
;;   :init (setq ffip-use-rust-fd t))


(provide 'init-emacs-search-finder)

;;; init-emacs-search-finder.el ends here
