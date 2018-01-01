;;; init-emms.el --- init for EMMS
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; emms prefix keybinding map
(unless (boundp 'emms-prefix)
  (define-prefix-command 'emms-prefix))
(define-key tools-prefix (kbd "e") 'emms-prefix)

;;; [ EMMS ]

(use-package emms
  :ensure t
  :load (emms-setup)
  :init
  (define-key emms-prefix (kbd "e") 'emms)
  (add-to-list 'display-buffer-alist
               '("^\\*EMMS Playlist\\*" (display-buffer-below-selected)))
  :config
  (emms-all)
  ;; [ players ]
  (emms-default-players)

  ;; [ Playlist ]
  ;; The name of emms-playlist-mode is *way* too long
  (add-hook 'emms-playlist-mode-hook #'(lambda () (setq mode-name "EMMS/playlist")))

  (setq emms-last-played-format-alist
        '(((emms-last-played-seconds-today) . "%a %H:%M")
	        (604800                           . "%a %H:%M") ; this week
	        ((emms-last-played-seconds-month) . "%d")
	        ((emms-last-played-seconds-year)  . "%m/%d")
	        (t                                . "%Y/%m/%d")))
  
  ;; Switch to either the radio buffer or the current EMMS playlist
  (defun my-emms-switch-to-current-playlist ()
    "Switch current playlist or start playlist."
    (interactive)
    (if (and (boundp 'emms-stream-playlist-buffer)
             (eq emms-stream-playlist-buffer emms-playlist-buffer))
        (switch-to-buffer emms-stream-buffer-name)
      (if (or (null emms-playlist-buffer)
              (not (buffer-live-p emms-playlist-buffer)))
          (error "No current Emms buffer")
        (switch-to-buffer emms-playlist-buffer))))

  ;; [ Track ]
  ;; (setq emms-track-description-function 'emms-track-simple-description)
  
  ;; [ Score ]
  (emms-score 1)

  ;; [ Encoding ]
  (setq emms-info-mp3info-coding-system '(utf-8 gbk)
        emms-cache-file-coding-system 'utf-8)

  ;; [ Streams: Radio, Podcasts ]

  ;; Switch to the radio buffer
  (defun my-emms-streams ()
    "Switch to streams buffer, if does not exists, then start emms-streams."
    (interactive)
    (let ((buf (get-buffer emms-stream-buffer-name)))
      (if buf
          (switch-to-buffer buf)
        (emms-streams))))

  ;; [ Key Bindings ]

  ;; [ MPD ] -- [M-x emms-player-mpd-connect]
  (require 'emms-player-mpd)

  (add-to-list 'emms-player-list 'emms-player-mpd t)
  
  (setq emms-player-mpd-server-name "127.0.0.1"
        emms-player-mpd-server-port "6600")

  ;; (setq emms-player-mpd-music-directory)
  (setq emms-player-mpd-sync-playlist t)

  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)

  ;; (global-set-key (kbd "Fn + <F10>") 'emms-player-mpd-pause)

  (unless (boundp 'emms-mpd-prefix)
    (define-prefix-command 'emms-mpd-prefix))
  (define-key emms-prefix (kbd "m") 'emms-mpd-prefix)

  (define-key emms-mpd-prefix (kbd "m") 'emms-player-mpd-connect)
  (define-key emms-mpd-prefix (kbd "c") 'emms-player-mpd-connect)
  (define-key emms-mpd-prefix (kbd "s") 'emms-player-mpd-start)
  (define-key emms-mpd-prefix (kbd "p") 'emms-player-mpd-play)
  ;; TODO: (emms-player-mpd-get-mpd-state nil #'ignore info)
  (define-key emms-mpd-prefix (kbd "SPC") 'emms-player-mpd-show)
  (define-key emms-mpd-prefix (kbd "P") 'emms-player-mpd-pause)
  (define-key emms-mpd-prefix (kbd "S") 'emms-player-mpd-stop)

  ;; [ mpv ]
  (use-package emms-player-mpv
    :ensure t
    :config
    (add-to-list 'emms-player-list 'emms-player-mpv)
    )
  )


(provide 'init-emms)

;;; init-emms.el ends here
