;;; init-tool-video.el --- init file for video processing -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-08-01 18:02:09 stardiviner>

;;; Commentary:



;;; Code:

;;; [ ffmpeg ] -- ffmpeg command wrapper for Emacs.

(use-package ffmpeg
  :load-path "~/Code/Emacs/ffmpeg.el"
  :commands (ffmpeg-cut-clip))



(provide 'init-tool-video)

;;; init-tool-video.el ends here
