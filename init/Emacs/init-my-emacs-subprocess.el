;;; init-my-emacs-subprocess.el --- init for Emacs subprocess managing.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ prodigy ] -- Manage external services from within Emacs.

(use-package prodigy
  :ensure t
  :config
  (setq prodigy-completion-system 'ivy
        prodigy-kill-process-buffer-on-stop nil
        )
  )



(provide 'init-my-emacs-subprocess)

;;; init-my-emacs-subprocess.el ends here
