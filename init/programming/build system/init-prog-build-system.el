;;; init-prog-build-system.el --- init for Build System.

;;; Commentary:



;;; Code:

(unless (boundp 'build-system-prefix)
  (define-prefix-command 'build-system-prefix))
(global-set-key (kbd "<f6>") 'build-system-prefix)

(require 'init-make)
(require 'init-cmake)

;;; [ build-helper ] -- Utilities to help build code.

(use-package build-helper
  :ensure t
  :defer t
  :bind (:map build-system-prefix
              ("<f5>" . compile)
              ("<f7>" . build-helper-run)
              ("<f8>" . build-helper-re-run)))

;;; [ build-status ] -- mode line build status indicator.

;;; TODO: need to learn how to use CI like Travis CI.
;; (use-package build-status
;;   :ensure t
;;   :init (add-hook 'prog-mode-hook #'build-status-mode))



(provide 'init-prog-build-system)

;;; init-prog-build-system.el ends here
