;;; init-tool-speak.el --- init Speak
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:

(unless (boundp 'speak-map)
  (define-prefix-command 'speak-map))
(define-key tools-prefix (kbd "s") 'speak-map)


;;; [ Emacspeak ]

;; (load "~/compile/Emacs/emacspeak/lisp/emacspeak-setup.el")


;;; [ Festival ] -- provides a simple interface into the festival speech synthesis program.

;; (use-package festival
;;   :quelpa (festival :fetcher github :repo "davep/festival.el")
;;   :commands (festival-start festival-stop
;;                             festival-say
;;                             festival-read-file festival-read-buffer
;;                             festival-read-region festival-read-word
;;                             festival-describe-function
;;                             festival-voice festival-spook))

;;; [ read-aloud ] -- A simple Emacs interface to TTS (text-to-speech) engines.

(use-package read-aloud
  :ensure t
  :init (setq read-aloud-engine "speech-dispatcher")
  :commands (read-aloud-buf read-aloud-this read-aloud-stop read-aloud-change-engine))

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
  :init (setq say-what-im-doing-shell-command (if (executable-find "mimic") "mimic" "espeak")
              say-what-im-doing-shell-command-options
              (if (equal say-what-im-doing-shell-command "mimic") "-t"))
  :commands (say-what-im-doing-mode)
  :config
  (add-to-list 'say-what-im-doing-common-commands 'move-beginning-of-line)
  (add-to-list 'say-what-im-doing-common-commands 'move-end-of-line))


;;; [ eloud ] -- A lightweight, interactive screen reader.

(use-package eloud
  :ensure t
  :init (setq eloud-espeak-path (cond
                                 ((string-equal system-type "gnu/linux")
                                  "/usr/bin/espeak")
                                 ((string-equal system-type "darwin")
                                  "/usr/local/bin/espeak")))
  :commands (eloud-mode))


(provide 'init-tool-speak)

;;; init-tool-speak.el ends here
