;;; init-my-tool-pomodoro.el --- init for Pomodoro technique.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------

(unless (boundp 'pomodoro-prefix)
  (define-prefix-command 'pomodoro-prefix))
(global-set-key (kbd "<f12>") 'pomodoro-prefix)
;; (define-key my-tools-prefix (kbd "C-p") 'pomodoro-prefix)


;;; [ tomatinho ] -- A simple and beautiful pomodoro technique timer that runs on Emacs.

(use-package tomatinho
  :ensure t
  :bind (:map pomodoro-prefix
              ("<f12>" . tomatinho)
              :map tomatinho-map
              ("n" . tomatinho-interactive-new-pomodoro)
              ("p" . tomatinho-interactive-deliberate-pause)
              ("<DEL>" . tomatinho-interactive-reset)
              ("TAB" . tomatinho-interactive-toggle-display)
              ("q" . tomatinho-interactive-kill-buffer)
              ("Q" . tomatinho-interactive-quit))
  :config
  ;; (setq tomatinho-bar-length 25)
  )


;;; ----------------------------------------------------------------------------

(provide 'init-my-tool-pomodoro)

;;; init-my-tool-pomodoro.el ends here
