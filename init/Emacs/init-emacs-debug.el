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

(use-package edebug
  :bind (:map emacs-lisp-mode-map ("C-c d e" . edebug-mode))
  :config
  ;; (setq edebug-trace t)
  ;; (add-to-list 'display-buffer-alist
  ;;              '("^\\*edebug-trace\\*" (display-buffer-below-selected)))
  ;;; show edebug trace result inline instead of echo-area.
  (defun my:edebug-previous-result ()
    "Print the previous result."
    (interactive)
    ;; (pos-tip-show edebug-previous-result 'popup-face) ; slow
    (popup-tip edebug-previous-result :truncate t :height 20 :width 45 :nostrip t :margin 1 :nowait nil)
    )
  (advice-add 'edebug-previous-result :override #'my:edebug-previous-result)
  )

;;; [ edebug-x ] -- Extensions for Edebug.

(use-package edebug-x
  :ensure t
  :preface
  (autoload 'color-darken-name "color.el")
  (autoload 'color-lighten-name "color.el")
  :bind (:map edebug-mode-map
              ("M-s" . edebug-x-show-data)
              ("M-b" . edebug-x-show-breakpoints)
              ("M-i" . edebug-x-show-instrumented))
  :config
  (set-face-attribute 'hi-edebug-x-stop nil
                      :reverse-video nil :foreground nil :overline nil
                      :background (cl-case (alist-get 'background-mode (frame-parameters))
                                    ('light "orange")
                                    ('dark "orange red")))
  (set-face-attribute 'hi-edebug-x-debug-line nil
                      :reverse-video nil :foreground nil :underline nil
                      :background (cl-case (alist-get 'background-mode (frame-parameters))
                                    ('light "pink")
                                    ('dark "hot pink")))

  (add-to-list 'display-buffer-alist
               '("^\\*Instrumented Functions\\*" (display-buffer-below-selected)))
  (add-to-list 'display-buffer-alist
               '("^\\*Edebug Breakpoints\\*" (display-buffer-below-selected)))
  )


;;; [ bug-hunter ] -- Hunt down errors in elisp files.

(use-package bug-hunter
  :ensure t
  :defer t
  :commands (bug-hunter-file bug-hunter-init-file))

;;; [ profiler ] -- Emacs buolt-in profiler.

;;; [ elp ] -- Emacs Lisp profiler.

(add-to-list 'display-buffer-alist
             '("\\*ELP Profiling Results\\*" . (display-buffer-below-selected)))

;;; [ trace ] -- tracing facility for Emacs Lisp functions.

;; (add-to-list 'display-buffer-alist
;;              '("\\*trace-output\\*" . (display-buffer-below-selected)))


(provide 'init-emacs-debug)

;;; init-emacs-debug.el ends here
