;;; init-my-tool-screencast.el --- init for Screencast usage
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ command-log-mode ] -- log commands to buffer

;;; This package is a fork of mwe-log-commands.

;;; Usage:
;;
;; 1. enable command-log-mode at first.
;;    [M-x command-log-mode] :: open and close buffer.
;; - `clm/' :: prefix.
;; 2. [M-x clm/toggle-command-log-buffer] :: toggle command log buffer.

;; (require 'command-log-mode)
(autoload 'command-log-mode "command-log-mode" nil t)
(autoload 'clm/toggle-command-log-buffer "command-log-mode" nil t)

(setq command-log-mode-is-global t
      command-log-mode-auto-show t
      command-log-mode-key-binding-open-log "C-c t L"
      command-log-mode-open-log-turns-on-mode t
      command-log-mode-window-size 40
      command-log-mode-window-font-size 2
      )

;; (add-hook 'org-mode-hook 'command-log-mode)

(define-key my-screenshot-map (kbd "L") 'command-log-mode)


;;; [ mwe-log-commands ]


;;; [ capture ] -- record video from desktop (use FFmpeg or aconv as backend)

;;; Usage:
;;
;; - [M-x capture-mode] ::
;; - `capture-run-mode' ::
;;
;; keybindings
;; - g :: update buffer
;; - When on audio string:
;;   - w :: copy device title (Built-in Audio Analog Stereo)
;;   - W :: copy device name (alsa_input.pci-0000_00_1b.0.analog-stereo)

;; FIXME:
;; (require 'capture)

;; (setq capture-video-dest-dir "~/screencasts/SORT/")
;; (global-set-key (kbd "<s-f12>") 'capture-run-mode)

;; (defun my-capture-presets ()
;;   "Make my presets for capturing."
;;   (interactive)
;;   (capture-presets-clear)
;;   ;; - 454, 74               - x and y offset
;;   ;; - 1280, 720             - width and height of the video
;;   ;; - 15                    - frames per second
;;   ;; - "webm"                - extension for a filename
;;   ;; - ""                    - additional arguments for ffmpeg (avconv)
;;   ;; - "1280px (no audio)"   - preset title
;;   (capture-add-preset 454 74 1280 720 15 "webm"
;;                       ""
;;                       "1280px (no audio)"))
;; (my-capture-presets)


;;; [ camcorder ] -- Tool for capturing screencasts directly from Emacs (use FFmpeg as backend)

;;; Usage:
;;
;; - [M-x camcorder-record] :: A new smaller frame will popup and recording starts.
;; - [F12] :: When you’re finished, hit F12 and wait for the conversion to FINISH.
;; - [F11] :: You can even PAUSE the recording with F11!
;; - [M-x camcorder-mode] :: If you want to record without a popup frame.

(require 'camcorder)

(define-key my-screenshot-map (kbd "r") 'camcorder-record)


(provide 'init-my-tool-screencast)

;;; init-my-tool-screencast.el ends here
