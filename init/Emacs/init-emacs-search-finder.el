;;; init-emacs-search-finder.el --- init for command find
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'find-prefix)
  (define-prefix-command 'find-prefix))

(define-key search-prefix (kbd "f") 'find-prefix)

(use-package helm
  :ensure t
  :bind (:map find-prefix ("h" . helm-find)))

;;; [ fd-dired ] -- find-dired alternative using fd.

(use-package fd-dired
  :ensure t
  :ensure-system-package fd
  :defer t
  :commands (fd-dired)
  :bind (:map find-prefix ("f" . fd-dired))
  :config (add-to-list 'display-buffer-alist '("^\\*Fd\\*" (display-buffer-below-selected))))


(provide 'init-emacs-search-finder)

;;; init-emacs-search-finder.el ends here
