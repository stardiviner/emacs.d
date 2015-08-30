;;; init-my-tool-speak.el --- init Speak
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:

;;; [ Emacspeak ]

;; http://www.emacswiki.org/emacs/EmacSpeak

;; Usage:

(load "~/compile/Emacs/emacspeak/lisp/emacspeak-setup.el")


;;; [ Festival ]

(require 'festival)

(setq festival-auto-start t ; should festival start when any of the functions are called.
      festival-buffer "*festival*"
      festival-default-audio-mode 'async
      festival-default-voice 'festival-voice-english-male
      festival-program "/usr/bin/festival"
      festival-voices-alist '(("english-fair" . festival-voice-english-fair)
                              ("english-male" . festival-voice-english-male)
                              ("us-male" . festival-voice-US-male))
      )

;; (run-festival)              ; FIXME: (void-variable inferior-festivalr-mode-map)
;; (festival-start-process)                ; start process at background of Emacs.


;;; festival extension

(require 'thingatpt)

;; FIXME: this function seems does not work.
(defun festival-read ()
  "Read current word that at point by Festival."
  (interactive)
  (if (use-region-p)
      (let ((region (buffer-substring-no-properties (region-beginning) (region-end))))
        (festival-say-region region)
        (message "Festival reading (region) ..."))
    (let ((word (thing-at-point 'word)))
      (festival-say word)
      (message "Festival reading (word): %s" word))
    )
  )

(unless (boundp 'speak-map)
  (define-prefix-command 'speak-map))
(define-key my-tools-prefix (kbd "s") 'speak-map)

(define-key speak-map (kbd "s") 'festival-read)
(define-key speak-map (kbd "r") 'festival-read-region)
(define-key speak-map (kbd "b") 'festival-read-buffer)
(define-key speak-map (kbd "f") 'festival-read-file)
(define-key speak-map (kbd "i") 'festival-say)






(provide 'init-my-tool-speak)

;;; init-my-tool-speak.el ends here
