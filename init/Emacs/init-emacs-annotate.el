;;; init-emacs-annotate.el --- init for Emacs annotate.

;;; Commentary:



;;; Code:

;;; [ annotate ] -- add annotations to arbitrary files without changing the files themselves.

;; (use-package annotate
;;   :ensure t
;;   :defer t
;;   :commands (annotate-mode
;;              annotate-annotate annotate-next-annotation annotate-previous-annotation
;;              annotate-clear-annotations annotate-integrate-annotations
;;              annotate-export-annotations annotate-save-annotations)
;;   :init
;;   ;; (dolist (hook '(emacs-lisp-mode-hook
;;   ;;                 lisp-mode-hook
;;   ;;                 clojure-mode-hook
;;   ;;                 python-mode-hook
;;   ;;                 java-mode-hook))
;;   ;;   (add-hook hook #'annotate-mode))
;;   (setq annotate-file (concat user-emacs-directory ".annotations")))




(provide 'init-emacs-annotate)

;;; init-emacs-annotate.el ends here
