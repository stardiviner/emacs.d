;;; init-my-tool-audio.el --- init for Audio in Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ ecasound ] -- command-line multitrack audio processor.

(add-to-list 'load-path "/usr/share/ecasound/")

(require 'ecasound)

(define-key my-tools-prefix (kbd "A") 'ecasound)

;;; ----------------------------------------------------------------------------

(provide 'init-my-tool-audio)

;;; init-my-tool-audio.el ends here
