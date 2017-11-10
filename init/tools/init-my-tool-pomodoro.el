;;; init-my-tool-pomodoro.el --- init for Pomodoro technique.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'pomodoro-prefix)
  (define-prefix-command 'pomodoro-prefix))
(global-set-key (kbd "<f12>") 'pomodoro-prefix)
;; (define-key my-tools-prefix (kbd "C-p") 'pomodoro-prefix)

;;; [ pomidor ] -- Pomidor is a simple and cool pomodoro technique timer.

(use-package pomidor
  :ensure t
  :bind (:map pomodoro-prefix
              ("<f12>" . pomidor)
              :map pomidor-mode-map
              ("q" . pomidor-quit))
  :config
  ;; disable sounds
  (setq pomidor-sound-tick nil
        pomidor-sound-tack nil
        ;; pomidor-sound-overwork nil
        )
  )


(provide 'init-my-tool-pomodoro)

;;; init-my-tool-pomodoro.el ends here
