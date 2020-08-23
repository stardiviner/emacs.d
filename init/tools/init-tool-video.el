;;; init-tool-video.el --- init file for video processing -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-08-01 18:02:09 stardiviner>

;;; Commentary:



;;; Code:

;;; [ mpv ] -- control mpv for easy note-taking. A potpourri of helper functions to control a mpv process via its IPC interface.

(use-package mpv
  :ensure t
  :commands (mpv-play mpv-pause mpv-kill
                      mpv-seek-forward mpv-seek-backward
                      mpv-speed-increase mpv-speed-decrease
                      mpv-insert-playback-position mpv-seek-to-position-at-point))

;;; [ ffmpeg ] -- ffmpeg command wrapper for Emacs.

(use-package ffmpeg
  :quelpa (ffmpeg :fetcher github :repo "stardiviner/ffmpeg.el")
  :commands (ffmpeg-cut-clip))

;;; [ vlc ] -- VideoLAN VLC Media Player Control through the Web Interface's API.

(use-package vlc
  :ensure t)



(provide 'init-tool-video)

;;; init-tool-video.el ends here
