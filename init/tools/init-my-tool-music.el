;;; init-my-tool-music.el --- init EMMS
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:


(unless (boundp 'my-music-prefix)
  (define-prefix-command 'my-music-prefix))

(define-key my-tools-prefix (kbd "M") 'my-music-prefix)


;;; [ MPC ] -- Emacs built-in Music Player

(use-package mpc
  :ensure t
  :defer t
  :init
  (advice-add 'mpc :after
              (lambda ()
                (switch-to-buffer-other-window "*MPC-Songs*")))

  (define-key my-music-prefix (kbd "M") 'mpc)
  (define-key my-music-prefix (kbd "y") 'mpc-toggle-single)
  (define-key my-music-prefix (kbd "r") 'mpc-toggle-repeat)
  (define-key my-music-prefix (kbd "s") 'mpc-toggle-shuffle)
  (define-key my-music-prefix (kbd "t") 'mpc-toggle-play)

  :config
  (defun my-mpc-songs-search ()
    (interactive)
    (call-interactively 'mpc-songs-search)
    (switch-to-buffer-other-window "*MPC-Songs*"))
  
  (define-key mpc-mode-map (kbd "l") 'my-mpc-songs-search)
  (define-key mpc-mode-map (kbd "L") 'mpc-songs-kill-search)

  (define-key mpc-mode-map (kbd "y") 'mpc-toggle-single)
  (define-key mpc-mode-map (kbd "r") 'mpc-toggle-repeat)
  (define-key mpc-mode-map (kbd "s") 'mpc-toggle-shuffle)
  (define-key mpc-mode-map (kbd "t") 'mpc-toggle-play)
  )

;;; [ dionysos ] -- A simple music player for Emacs by support backends like: MPD, VLC, MPlayer.

(use-package dionysos
  :ensure t
  :config
  (setq dionysos-backend 'mpd
        dionysos-notify-p t
        dionysos-volume-cmd 'pamixer)
  )


;; (require 'init-my-tool-music-emms)



(provide 'init-my-tool-music)

;;; init-my-tool-music.el ends here
