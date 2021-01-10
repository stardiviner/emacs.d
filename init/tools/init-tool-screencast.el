;;; init-tool-screencast.el --- init for Screencast usage
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'screencast-prefix)
  (define-prefix-command 'screencast-prefix))
(define-key tools-prefix (kbd "M-s") 'screencast-prefix)

;;; [ keycast ] -- Show current command and its key in the mode line.

(use-package keycast
  :ensure t
  :defer t
  :custom (keycast-remove-tail-elements nil)
  :commands (keycast-mode keycast-log-mode)
  :bind (:map screencast-prefix
              ("k" . keycast-mode)
              ("K" . keycast-log-mode))
  :custom-face
  (keycast-key ((t (:weight bold))))
  (keycast-command ((t (:slant italic))))
  :init
  (when (fboundp 'doom-modeline-mode)
    (define-minor-mode keycast-mode
      "Show current command and its key binding in the mode line."
      :global t
      (if keycast-mode
          (add-hook 'pre-command-hook 'keycast--update)
        (remove-hook 'pre-command-hook 'keycast--update)))
    
    (setq keycast-window-predicate 'selected-window)
    
    (defun doom-modeline--toggle-keycast (orig-func &rest args)
      "Toggle keycast-mode fragment in doom-modeline."
      (if keycast-mode
          (setq global-mode-string (delete '("" mode-line-keycast " ") global-mode-string))
        (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (apply orig-func args))
    
    (advice-add 'keycast-mode :around #'doom-modeline--toggle-keycast)))

;;; [ keypression ] -- Keystroke visualizer.

(use-package keypression
  :ensure t
  :commands (keypression-mode)
  :custom ((keypression-cast-command-name t)
           (keypression-combine-same-keystrokes t)))

;;; [ gif-screencast ] -- one-frame-per-action GIF recording for optimal quality/size ratio.

(use-package gif-screencast
  :ensure t
  :commands (gif-screencast)
  :init (setq gif-screencast-output-directory (expand-file-name "~/"))
  :config
  (with-eval-after-load 'gif-screencast
    (define-key gif-screencast-mode-map (kbd "<f12>") 'gif-screencast-toggle-pause)
    (define-key gif-screencast-mode-map (kbd "<f11>") 'gif-screencast-stop)))

;;; [ camcorder ] -- Tool for capturing screencasts directly from Emacs (use FFmpeg as backend)

(use-package camcorder
  :ensure t
  :defer t
  :commands (camcorder-mode camcorder-record)
  :bind (:map screencast-prefix
              ("r" . camcorder-mode)
              ("R" . camcorder-record))
  :config
  ;; (setq camcorder-recording-command
  ;;       '("recordmydesktop" " --fps 20 --no-sound --windowid " window-id " -o " file))
  
  (setq camcorder-frame-parameters ; see `make-frame', `default-frame-alist'
        '((name . "camcorder.el Recording - F12 to Stop - F11 to Pause/Resume")
          (height . 150)
          (width . 90)
          ;; (top .  80)
          (minibuffer . t)
          )
        ;; window-id-offset 0
        ))


(provide 'init-tool-screencast)

;;; init-tool-screencast.el ends here
