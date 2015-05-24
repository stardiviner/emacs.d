;;; init-my-tool-emms.el --- init EMMS
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Usage

;;; Code:

;;; [ EMMS ]

(require 'emms)
(require 'emms-setup)

(require 'emms-info)
;; (require 'emms-playing-time)
(require 'emms-playlist-mode)
(require 'emms-volume)
(require 'emms-score)
;; auto detect music files id3 tags encoding
(require 'emms-i18n)
;; auto-save and import last playlist
(require 'emms-history)

;; (require 'emms-streams)


;; emms prefix keybinding map
(unless (boundp 'my-emms-prefix-map)
  (define-prefix-command 'my-emms-prefix-map))
(define-key my-tools-prefix-map (kbd "e") 'my-emms-prefix-map)



(emms-standard)

;; (setq emms-player-list
;;       '(emms-player-mpg321 emms-player-ogg123
;;                            emms-player-mplayer-playlist emms-player-mplayer emms-player-vlc
;;                            ;; emms-player-mpd
;;                            ))

(emms-default-players)


;;; [ Playlist ]

;; The name of emms-playlist-mode is *way* too long
(add-hook 'emms-playlist-mode-hook
          #'(lambda ()
              (setq mode-name "EMMS/playlist")))


;;; [ Score ]
(emms-score 1)


;;; Encoding
(setq emms-info-mp3info-coding-system '(utf-8 gbk)
      emms-cache-file-coding-system 'utf-8)


;;; [ MPD ]
(require 'emms-player-mpd)

(setq emms-player-mpd-server-name "127.0.0.1"
      emms-player-mpd-server-port "6600")

;; (setq emms-player-mpd-music-directory)
(setq emms-player-mpd-sync-playlist t)

(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)

;; TODO capture the key code of [Fn + <F10>] to apply. also include next [F11] etc.
;; (global-set-key (kbd "Fn + <F10>") 'emms-player-mpd-pause)

(unless (boundp 'my-emms-mpd-prefix-map)
  (define-prefix-command 'my-emms-mpd-prefix-map))
(define-key my-emms-prefix-map (kbd "m") 'my-emms-mpd-prefix-map)

(define-key my-emms-mpd-prefix-map (kbd "c") 'emms-player-mpd-connect)
(define-key my-emms-mpd-prefix-map (kbd "p") 'emms-player-mpd-pause) ; toggle pause
(define-key my-emms-mpd-prefix-map (kbd "P")
  '(lambda ()
     (emms-player-mpd-play nil)))
(define-key my-emms-mpd-prefix-map (kbd "s") 'emms-player-mpd-stop)


;;; [ Playlist ]

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


;;; [ Streams: Radio, Podcasts ]

;; Switch to the radio buffer
(defun my-emms-streams ()
  "Switch to streams buffer, if does not exists, then start emms-streams."
  (interactive)
  (let ((buf (get-buffer emms-stream-buffer-name)))
    (if buf
        (switch-to-buffer buf)
      (emms-streams))))


;;; [ Key Bindings ]

;; TODO capture the key code of [Fn + <F10>] to apply. also include next [F11] etc.
;; (global-set-key (kbd "Fn + <F10>") 'emms-player-mpd-pause)




(provide 'init-my-tool-emms)

;;; init-my-tool-emms.el ends here
