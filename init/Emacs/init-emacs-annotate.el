;;; init-emacs-annotate.el --- init for Emacs annotate.

;;; Time-stamp: <2019-04-14 16:47:09 stardiviner>

;;; Commentary:



;;; Code:

;;; [ annotate ] -- add annotations to arbitrary files without changing the files themselves.

(use-package annotate
  :ensure t
  :defer t
  :commands (annotate-mode
             annotate-annotate annotate-next-annotation annotate-previous-annotation
             annotate-clear-annotations annotate-integrate-annotations
             annotate-export-annotations annotate-save-annotations)
  :init (annotate-mode)
  (setq annotate-file (concat user-emacs-directory ".annotations")))




(provide 'init-emacs-annotate)

;;; init-emacs-annotate.el ends here
