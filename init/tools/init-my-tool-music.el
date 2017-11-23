;;; init-my-tool-music.el --- init EMMS
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:


(unless (boundp 'music-prefix)
  (define-prefix-command 'music-prefix))

(define-key tools-prefix (kbd "M") 'music-prefix)

;;; [ MPD ]

(require 'init-mpd)

;;; [ EMMS ]

(require 'init-emms)

;;; [ PulseAudio ]

(use-package pulseaudio-control
  :ensure t
  :config
  ;; keybindings
  ;; (pulseaudio-control-default-keybindings)
  ;; (global-set-key (kbd "C-x /") 'pulseaudio-control-map)
  (define-key music-prefix (kbd "a") 'pulseaudio-control-map)
  )


(provide 'init-my-tool-music)

;;; init-my-tool-music.el ends here
