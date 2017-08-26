;;; init-my-prog-document-assistant.el --- code assistant
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ inline-docs ] -- show contextual docs with inline style.

(use-package inline-docs
  :ensure t
  :config
  (setq inline-docs-position 'above)
  )

;;; [ quick-peek ] -- An inline pop-up library for Emacs Lisp.

(use-package quick-peek
  :ensure t
  :commands (quick-peek-hide)
  )


(provide 'init-my-prog-document-assistant)

;;; init-my-prog-document-assistant.el ends here
