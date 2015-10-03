;;; init-my-tool-music.el --- init EMMS
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:


(unless (boundp 'my-music-prefix)
  (define-prefix-command 'my-music-prefix))

(define-key my-tools-prefix (kbd "M") 'my-music-prefix)


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
(unless (boundp 'my-emms-prefix)
  (define-prefix-command 'my-emms-prefix))
(define-key my-tools-prefix (kbd "e") 'my-emms-prefix)


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



;;; [ MPD ]

;;; Usage:
;;
;; - [M-x emms-player-mpd-connect]

(require 'emms-player-mpd)

(setq emms-player-mpd-server-name "127.0.0.1"
      emms-player-mpd-server-port "6600")

;; (setq emms-player-mpd-music-directory)
(setq emms-player-mpd-sync-playlist t)

(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)

;; TODO capture the key code of [Fn + <F10>] to apply. also include next [F11] etc.
;; (global-set-key (kbd "Fn + <F10>") 'emms-player-mpd-pause)

(unless (boundp 'my-emms-mpd-prefix)
  (define-prefix-command 'my-emms-mpd-prefix))
(define-key my-emms-prefix (kbd "m") 'my-emms-mpd-prefix)

(define-key my-emms-mpd-prefix (kbd "c") 'emms-player-mpd-connect)
(define-key my-emms-mpd-prefix (kbd "p") 'emms-player-mpd-pause) ; toggle pause
(define-key my-emms-mpd-prefix (kbd "P")
  '(lambda ()
     (emms-player-mpd-play nil)))
(define-key my-emms-mpd-prefix (kbd "s") 'emms-player-mpd-stop)


;;; [ mingus ] -- MPD client

;;; Usage:
;;
;; - `mingus'
;; - `mingus-stays-home'

(use-package mingus
  :config
  (autoload 'mingus "mingus-stays-home" nil t)
  ;; (autoload 'mingus "mingus" nil t)

  (define-key my-music-prefix (kbd "M") 'mingus)
  (define-key my-music-prefix (kbd "l") 'mingus-playlist)
  (define-key my-music-prefix (kbd "b") 'mingus-browse)
  (define-key my-music-prefix (kbd "t") 'mingus-toggle)
  (define-key my-music-prefix (kbd "n") 'mingus-next)
  (define-key my-music-prefix (kbd "p") 'mingus-prev)
  (define-key my-music-prefix (kbd "s") 'mingus-search)
  (define-key my-music-prefix (kbd "q") 'mingus-query)
  )



;; (require 'init-my-tool-music-bongo)



(provide 'init-my-tool-music)

;;; init-my-tool-music.el ends here
