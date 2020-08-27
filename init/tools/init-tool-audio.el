;;; init-tool-audio.el --- init for Audio in Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
(unless (boundp 'audio-prefix)
  (define-prefix-command 'audio-prefix))
(define-key tools-prefix (kbd "C-a") 'audio-prefix)

(defun sox-record-sound ()
  "Record sound with SoX command in Emacs."
  (interactive)
  (let ((kill-buffer-query-functions nil)
        (buffer (shell "*SoX recording*")))
    (comint-simple-send buffer "sox -t alsa default ~/sox-recording.wav")
    (sit-for 2))
  (message "Recording sound to file '~/sox-recording.wav' in background."))

(add-to-list 'display-buffer-alist '("^\\*SoX recording\\*" . (display-buffer-below-selected)))

(define-key audio-prefix (kbd "r") 'sox-record-sound)

;;; [ ecasound ] -- command-line multitrack audio processor.

;; (use-package ecasound
;;   :load-path "/usr/share/ecasound/"
;;   :bind (:map audio-prefix ("e" . ecasound)))


;;; [ SoX.el ] -- helper for interacting with SoX, the Swiss Army knife of sound processing programs.

;; (use-package sox
;;   :quelpa (sox :fetcher github :repo "vxe/SoX.el")
;;   :commands (sox)
;;   :init (add-to-list 'display-buffer-alist '("^\\*sox\\*.*" . (display-buffer-below-selected))))

;;; ----------------------------------------------------------------------------

(provide 'init-tool-audio)

;;; init-tool-audio.el ends here
