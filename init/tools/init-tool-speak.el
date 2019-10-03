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

;; (leaf festival
;;   :el-get (festival :url "https://github.com/davep/festival.el.git")
;;   :commands (festival-start festival-stop
;;                             festival-say
;;                             festival-read-file festival-read-buffer
;;                             festival-read-region festival-read-word
;;                             festival-describe-function
;;                             festival-voice festival-spook))

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
  :init (setq eloud-espeak-path (cond
				 ((string-equal system-type "gnu/linux")
				  "/usr/bin/espeak")
				 ((string-equal system-type "darwin")
				  "/usr/local/bin/espeak")))
  :config (eloud-mode 1))


;;; [ read-aloud ] -- A simple Emacs interface to TTS (text-to-speech) engines.

(use-package read-aloud
  :ensure t
  :config
  (setq read-aloud-engine "speech-dispatcher"))


(provide 'init-tool-speak)

;;; init-tool-speak.el ends here
