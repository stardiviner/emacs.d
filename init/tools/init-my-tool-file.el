;;; init-my-tool-file.el --- init for Files Handling
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ pandoc-mode ] -- pandoc-mode is an Emacs mode for interacting with Pandoc.

;;; Usage:
;;
;; - [C-c /] :: the main keybindings dispatcher.

(use-package pandoc-mode
  :ensure t
  :config
  ;; (add-hook 'markdown-mode-hook 'pandoc-mode)
  ;;
  ;; This function checks if a default settings file exists for the file you're
  ;; opening and only turns on pandoc-mode if it finds one.
  ;;
  ;; (add-hook 'markdown-mode-hook 'conditionally-turn-on-pandoc)
  ;;
  ;; Additionally, if you want to automatically load the default pandoc-mode
  ;; settings file for the file you're opening, you can add the following to
  ;; your init file: The function pandoc-load-default-settings checks if a
  ;; default settings file exists for the file being loaded and reads its
  ;; settings if it finds one.
  ;;
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  )



(provide 'init-my-tool-file)

;;; init-my-tool-file.el ends here
