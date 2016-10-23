;;; init-my-tool-audio.el --- init for Audio in Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
(unless (boundp 'audio-prefix)
  (define-prefix-command 'audio-prefix))
(define-key my-tools-prefix (kbd "C-a") 'audio-prefix)


;;; [ ecasound ] -- command-line multitrack audio processor.

(use-package ecasound
  :load-path "/usr/share/ecasound/"
  :bind (:map audio-prefix
              ("e" . ecasound))
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-tool-audio)

;;; init-my-tool-audio.el ends here
