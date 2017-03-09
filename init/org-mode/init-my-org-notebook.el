;;; init-my-org-notebook.el --- init Org-mode for Notebook.

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------

(unless (boundp 'org-notebook-prefix)
  (define-prefix-command 'org-notebook-prefix))

(define-key my-org-prefix (kbd "n") 'org-notebook-prefix)

;;; [ org-notebook ] -- A package to ease the use of org-mode as a notebook.

(use-package org-notebook
  :ensure t
  :config
  (setq org-notebook-drawing-program
        (cond
         ((executable-find "kolourpaint") "kolourpaint")
         ((executable-find "mypaint") "mypaint")
         ((executable-find "krita") "krita")
         ((executable-find "gimp") "gimp")))
  (setq org-notebook-image-width 300)

  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-notebook-prefix (kbd "i") 'org-notebook-insert-image)
              (define-key org-notebook-prefix (kbd "n") 'org-notebook-new-notebook)))
  )


;;; ----------------------------------------------------------------------------

(provide 'init-my-org-notebook)

;;; init-my-org-notebook.el ends here
