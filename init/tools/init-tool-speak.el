;;; init-tool-speak.el --- init Speak
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:

(unless (boundp 'speak-map)
  (define-prefix-command 'speak-map))
(define-key tools-prefix (kbd "s") 'speak-map)


;;; [ Emacspeak ]

;; (load "~/compile/Emacs/emacspeak/lisp/emacspeak-setup.el")


;;; [ Festival ]

;; (require 'festival)
;;
;; Command: `run-festival'
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
;;
;; (require 'thingatpt)
;;
;; (defun festival-read ()
;;   "Read current word that at point by Festival."
;;   (interactive)
;;   (if (use-region-p)
;;       (let ((region (buffer-substring-no-properties (region-beginning) (region-end))))
;;         (festival-say-region region)
;;         (message "Festival reading (region) ..."))
;;     (let ((word (thing-at-point 'word)))
;;       (festival-say word)
;;       (message "Festival reading (word): %s" word))
;;     )
;;   )
;;
;; (define-key speak-map (kbd "s") 'festival-read)
;; (define-key speak-map (kbd "r") 'festival-read-region)
;; (define-key speak-map (kbd "b") 'festival-read-buffer)
;; (define-key speak-map (kbd "f") 'festival-read-file)
;; (define-key speak-map (kbd "i") 'festival-say)

;;; [ ekho ] -- Ekho (余音) is a free, open source and multilingual text-to-speech (TTS)

;; It supports Cantonese (Chinese dialect spoken in Hong Kong and part of
;; Guangdong province), Mandarin (standard Chinese), Toisanese, Zhaoan Hakka (a
;; dialect in Taiwan), Tibetan, Ngangien (an ancient Chinese before Yuan
;; Dynasty) and Korean (in trial).
;;
;; (async-shell-command "ekho \"你好\"")

;;; [ say-what-im-doing ] -- dictate what you're doing with text to speech.

(use-package say-what-im-doing
  :ensure t
  :ensure-system-package (mimic . "yaourt -S --noconfirm mimic")
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

  (say-what-im-doing-mode 1)
  )


;;; [ eloud ] -- A lightweight, interactive screen reader.

(use-package eloud
  :ensure t
  :ensure-system-package espeak
  :config
  (setq eloud-espeak-path (cond
                           ((string-equal system-type "gnu/linux")
                            "/usr/bin/espeak")
                           ((string-equal system-type "darwin")
                            "/usr/local/bin/espeak")))

  (eloud-mode 1)
  )


;;; [ read-aloud ] -- A simple Emacs interface to TTS (text-to-speech) engines.

(use-package read-aloud
  :ensure t
  :ensure-system-package (spd-say . "sudo pacman -S --noconfirm speech-dispatcher")
  :config
  (setq read-aloud-engine "speech-dispatcher")
  )


(provide 'init-tool-speak)

;;; init-tool-speak.el ends here
