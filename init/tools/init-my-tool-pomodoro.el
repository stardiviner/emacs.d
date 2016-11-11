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
              ("<f12>" . tomatinho))
  )


;;; ----------------------------------------------------------------------------

(provide 'init-my-tool-pomodoro)

;;; init-my-tool-pomodoro.el ends here
