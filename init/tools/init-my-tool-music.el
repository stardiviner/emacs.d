;;; init-my-tool-music.el --- init EMMS
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:


(unless (boundp 'my-music-prefix)
  (define-prefix-command 'my-music-prefix))

(define-key my-tools-prefix (kbd "M") 'my-music-prefix)

;;; [ MPD ]

(require 'init-mpd)

;;; [ EMMS ]

;; (require 'init-my-tool-music-emms)

;;; [ PulseAudio ]

(use-package pulseaudio-control
  :ensure t
  :config
  ;; keybindings
  ;; (pulseaudio-control-default-keybindings)
  ;; (global-set-key (kbd "C-x /") 'pulseaudio-control-map)
  (define-key my-music-prefix (kbd "a") 'pulseaudio-control-map)
  )


(provide 'init-my-tool-music)

;;; init-my-tool-music.el ends here
