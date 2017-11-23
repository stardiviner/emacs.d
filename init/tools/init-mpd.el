;;; init-mpd.el --- init for MPD

;;; Commentary:



;;; Code:

;;; [ MPC ] -- Emacs built-in Music Player

(use-package mpc
  :ensure t
  :config
  (setq mpc-mpd-music-directory "~/Music/")
  ;; Assign shortcut [Win+M] for Emacs MPC in KDE.
  (setq mpc-frame-alist '((name . "MPC") (tool-bar-lines . 0)
                          ;; (font . "WenQuanYi Micro Hei Mono")
                          ;; (alist-get 'height (frame-parameters))
                          ))
  
  (define-key mpc-mode-map (kbd "s") 'mpc-songs-search)
  (define-key mpc-mode-map (kbd "L") 'mpc-songs-kill-search)
  (define-key mpc-mode-map (kbd "p") 'mpc-toggle-play)
  (define-key mpc-mode-map (kbd "M-y") 'mpc-toggle-single)
  (define-key mpc-mode-map (kbd "M-r") 'mpc-toggle-repeat)
  (define-key mpc-mode-map (kbd "M-s") 'mpc-toggle-shuffle)

  (define-key music-prefix (kbd "p") 'mpc-toggle-play)
  (define-key music-prefix (kbd "y") 'mpc-toggle-single)
  (define-key music-prefix (kbd "r") 'mpc-toggle-repeat)
  (define-key music-prefix (kbd "s") 'mpc-toggle-shuffle)
  
  (defun my/mpc ()
    (interactive)
    (select-frame (make-frame mpc-frame-alist))
    (mpc))

  (define-key music-prefix (kbd "M") 'my/mpc)
  
  ;; auto start MPC frame.
  ;; (funcall-interactively 'my/mpc)
  )

;;; [ dionysos ] -- A simple music player for Emacs by support backends like: MPD, VLC, MPlayer.

(use-package dionysos
  :ensure t
  :config
  (setq dionysos-backend 'mpd
        dionysos-notify-p t
        dionysos-volume-cmd 'pamixer)
  )



(provide 'init-mpd)

;;; init-mpd.el ends here
