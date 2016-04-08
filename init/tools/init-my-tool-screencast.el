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

(use-package command-log-mode
  :ensure t
  :init
  ;; FIX: disable default global keybinding [C-c o]
  (setq command-log-mode-key-binding-open-log nil)
  :config
  (setq command-log-mode-auto-show t
        command-log-mode-is-global nil
        command-log-mode-open-log-turns-on-mode t
        ;; command-log-mode-window-size 40
        command-log-mode-window-font-size 2
        )

  ;; (set-face-attribute 'command-log-key nil
  ;;                     :background "gray" :foreground "red"
  ;;                     )
  ;; (set-face-attribute 'command-log-command nil
  ;;                     :background nil :foreground "black"
  ;;                     )

  (define-key my-screenshot-map (kbd "M-k") 'clm/toggle-command-log-buffer)
  (define-key my-screenshot-map (kbd "k") 'command-log-mode)
  (define-key my-screenshot-map (kbd "K") 'global-command-log-mode)
  )


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

(use-package capture
  ;; :ensure t
  ;; :config
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
  )


;;; [ camcorder ] -- Tool for capturing screencasts directly from Emacs (use FFmpeg as backend)

;;; Usage:
;;
;; - [M-x camcorder-mode] :: If you want to record without a popup frame.
;; - [M-x camcorder-record] :: A new smaller frame will popup and recording starts.
;; - [F11] :: You can even PAUSE the recording with F11!
;; - [F12] :: When you’re finished, hit F12 and wait for the conversion to FINISH.

(use-package camcorder
  :ensure t
  :config
  ;; (setq camcorder-recording-command
  ;;       '("recordmydesktop" " --fps 20 --no-sound --windowid " window-id " -o " file))
  
  (setq camcorder-frame-parameters ; see `make-frame', `default-frame-alist'
        '((name . "camcorder.el Recording - F12 to Stop - F11 to Pause/Resume")
          (height . 150)
          (width . 90)
          ;; (top .  80)
          (minibuffer . t)
          )
        ;; window-id-offset -4
        )

  (define-key my-screenshot-map (kbd "r") 'camcorder-mode)
  (define-key my-screenshot-map (kbd "R") 'camcorder-record)
  )


(provide 'init-my-tool-screencast)

;;; init-my-tool-screencast.el ends here
