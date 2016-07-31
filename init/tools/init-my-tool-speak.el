;;; init-my-tool-speak.el --- init Speak
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:

;;; [ Emacspeak ]

;; http://www.emacswiki.org/emacs/EmacSpeak

;; Usage:
;;
;; - [C-e] :: prefix

;; (load "~/compile/Emacs/emacspeak/lisp/emacspeak-setup.el")


;;; [ Festival ]

;; (require 'festival)
;;
;; (setq festival-auto-start t
;;       festival-buffer "*festival*"
;;       festival-default-audio-mode 'async
;;       festival-default-voice 'festival-voice-english-male
;;       festival-program "/usr/bin/festival"
;;       festival-voices-alist '(("english-fair" . festival-voice-english-fair)
;;                               ("english-male" . festival-voice-english-male)
;;                               ("us-male" . festival-voice-US-male))
;;       )


;;; festival extension

(require 'thingatpt)

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


;;; [ say-what-im-doing ] -- dictate what you're doing with text to speech.

(use-package say-what-im-doing
  :ensure t
  :config
  (setq say-what-im-doing-shell-command (if (executable-find "mimic")
                                            "mimic"
                                          "espeak")
        say-what-im-doing-shell-command-options
        (if (equal say-what-im-doing-shell-command "mimic")
            "-t")
        )
  
  
  (add-to-list 'say-what-im-doing-common-commands 'move-beginning-of-line)
  (add-to-list 'say-what-im-doing-common-commands 'move-end-of-line)
  ;; (say-what-im-doing-mode)
  )


;;; [ eloud ] -- A lightweight, interactive screen reader.

(use-package eloud
  :ensure t
  :config
  (setq eloud-espeak-path (cond
                           ((string-equal system-type "gnu/linux")
                            "/usr/bin/espeak")
                           ((string-equal system-type "darwin")
                            "/usr/local/bin/espeak")))

  ;; (eloud-mode 1)
  )


(provide 'init-my-tool-speak)

;;; init-my-tool-speak.el ends here
