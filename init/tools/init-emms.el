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
  :defer t
  :custom (emms-source-file-default-directory "~/Music/")
  :bind (:map emms-prefix ("e" . emms))
  :init (add-to-list 'display-buffer-alist '("^\\*EMMS Playlist\\*" . (display-buffer-below-selected)))
  :config
  (emms-all)
  ;; [ players ]
  (emms-default-players)
  ;; play:
  ;; next: 'emms-next-noerror
  ;; random: 'emms-random
  ;; only play current song: 'emms-stop
  (setq emms-player-next-function 'emms-next-noerror)

  ;; [ Playlist ]
  ;; format
  (setq emms-browser-info-title-format "%i%T %t - %a"
        emms-browser-playlist-info-title-format emms-browser-info-title-format)

  ;; for playlist like `emms-player-mpd'.
  (setq emms-track-description-function 'my/emms-info-track-description)
  (defun my/emms-info-track-description (track)
    "Return a description of TRACK."
    (let ((artist (emms-track-get track 'info-artist))
          (title  (emms-track-get track 'info-title)))
      (cond
       ((and artist title)
        (format "[ %s ] - %s" artist title))
       (title
        title)
       (t
        (emms-track-simple-description track)))))
  
  ;; [ Track ]
  
  ;; [ Score ]
  (emms-score 1)

  ;; [ Encoding ]
  (require 'emms-info-mp3info)
  (setq emms-info-mp3info-coding-system '(utf-8 gbk)
        emms-cache-file-coding-system 'utf-8)

  ;; [ EMMS mode-line ]
  (defun my/emms-mode-line-info ()
    (let* ((track (emms-playlist-current-selected-track))
           ;; (description (emms-track-description (emms-playlist-current-selected-track)))
           (title (file-name-nondirectory (cdr (assoc 'name track)))))
      (format emms-mode-line-format
              (s-truncate
               (/ (/ (frame-width) 2) 4)
               title))))
  (setq emms-mode-line-mode-line-function 'my/emms-mode-line-info)
  
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
  ;; (setq emms-info-asynchronously nil)
  (define-key emms-playlist-mode-map (kbd "Q") 'emms-stream-quit) ; really quit EMMS Stream.
  ;; (setq emms-playlist-default-major-mode 'emms-mark-mode)

  ;; [ MPD ] -- [M-x emms-player-mpd-connect]
  (require 'emms-player-mpd)

  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd t)
  
  (setq emms-player-mpd-server-name "127.0.0.1"
        emms-player-mpd-server-port "6600")
  (setq emms-player-mpd-music-directory "~/Music/music/")

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

  ;; [ MPV ]
  (require 'emms-player-mpv)
  
  ;; [ BBT: SBaGen ]
  (define-emms-simple-player sbagen '(file) (emms-player-simple-regexp "sbg") "sbagen")
  )

;;; [ org-emms ] -- Play multimedia files from org-mode with EMMS.

(use-package org-emms
  :after emms
  :ensure t)

;; [ emms-bilibili ] -- Play Bilibili user favourite videos in EMMS.

;; (use-package emms-bilibili
;;   ;; :ensure t
;;   :quelpa (emms-bilibili :fetcher github :repo "stardiviner/emms-bilibili" :branch "develop")
;;   :defer t
;;   :after emms
;;   :commands (emms-bilibili)
;;   :custom ((emms-bilibili-mid (string-to-number (my/json-read-value my/account-file 'emms-bilibili)))
;;            (emms-bilibili-use-popup t)))


(provide 'init-emms)

;;; init-emms.el ends here
