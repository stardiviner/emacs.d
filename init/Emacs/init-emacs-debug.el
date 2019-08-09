;;; init-emacs-debug.el --- init for Emacs debug

;;; Commentary:


;;; Code:

;;; [ debug ] -- Emacs built-in debugger.

;; for Emacs startup freeze debug.
;; (setq debug-on-quit t)
;; (add-hook 'after-init-hook #'(lambda () (setq debug-on-quit nil)))
;; (setq debug-on-error t)
;; (add-hook 'after-init-hook #'(lambda () (setq debug-on-error nil)))

;;; [ Edebug ] -- Edebug is a source level debugger.

;; (use-package edebug
;;   :bind (:map emacs-lisp-mode-map ("C-c d e" . edebug-mode))
;;   :init (setq edebug-trace t)
;;   (add-to-list 'display-buffer-alist
;;                '("^\\*edebug-trace\\*" (display-buffer-below-selected))))

;;; [ edebug-x ] -- Extensions for Edebug.

(use-package edebug-x
  :ensure t
  :defer t
  :commands (edebug-x-mode)
  ;; :init (setq edebug-x-stop-point-overlay t)
  :config
  (set-face-attribute 'hi-edebug-x-stop nil
                      :reverse-video nil :foreground nil :overline nil
                      :background (cl-case (alist-get 'background-mode (frame-parameters))
                                    ('light "orange")
                                    ('dark "DarkMagenta")))
  (set-face-attribute 'hi-edebug-x-debug-line nil
                      :reverse-video nil :foreground nil :underline nil
                      :background (cl-case (alist-get 'background-mode (frame-parameters))
                                    ('light "pink")
                                    ('dark "DarkSlateGray")))

  (add-to-list 'display-buffer-alist
               '("^\\*Instrumented Functions\\*" (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^\\*Edebug Breakpoints\\*" (display-buffer-below-selected))))

;;; [ edebug-inline-result ] -- Show Edebug result inline.

(leaf edebug-inline-result
  :el-get (edebug-inline-result :url "https://github.com/stardiviner/edebug-inline-result.git")
  :commands (edebug-inline-result-show))

;;; [ bug-hunter ] -- Hunt down errors in elisp files.

(use-package bug-hunter
  :ensure t
  :defer t
  :commands (bug-hunter-file bug-hunter-init-file))


(provide 'init-emacs-debug)

;;; init-emacs-debug.el ends here
