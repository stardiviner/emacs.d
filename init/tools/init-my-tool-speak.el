;;; init-my-tool-speak.el --- init Speak
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:

;;; [ Emacspeak ]

;; http://www.emacswiki.org/emacs/EmacSpeak


;;; [ Festival ]

(require 'festival)

;; (run-festival)              ; FIXME: (void-variable inferior-festivalr-mode-map)
(festival-start-process)                ; start process at background of Emacs.


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
      (festival-say-string word)
      (message "Festival reading (word): %s" word))
    )
  )

(unless (boundp 'speak-map)
  (define-prefix-command 'speak-map))
(define-key my-tools-prefix-map (kbd "s") 'speak-map)

(define-key speak-map (kbd "s") 'festival-read)





(provide 'init-my-tool-speak)

;;; init-my-tool-speak.el ends here
