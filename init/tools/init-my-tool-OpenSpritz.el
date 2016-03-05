;;; init-my-tool-OpenSpritz.el --- init Spray for Emacs OpenSpritz implement.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; - https://www.gnu.org/philosophy/right-to-read.html

;;; Code:

;;; [ spray ] -- [Emacs] an elisp implementation of OpenSpritz

(use-package spray
  :ensure t
  :config
  (setq spray-wpm 250 ; words per minute
        spray-height 400 ; Height of characters
        spray-margin-top 2 ; Character margin at top of buffer. Characters are as big as spray text characters.
        spray-margin-left 4
        spray-ramp 2 ; Initial words before ramping up to full speed.
        spray-save-point t
        )

  (set-face-attribute 'spray-base-face nil
                      :inherit 'default
                      :foreground "black"
                      :background "white"
                      :family "DejaVu Sans"
                      )
  (set-face-attribute 'spray-accent-face nil
                      :inherit 'spray-base-face
                      :foreground "red"
                      )
  )


;;; [ speedread ] -- Aid to speedreading emacs buffers.

(use-package speedread
  ;; :ensure t
  :config
  (setq speedread-chars 20
        speedread-delay-milliseconds 300
        speedread-end-sentence-delay-milliseconds 500
        speedread-final-delay-milliseconds 2000
        speedread-top-window-size 5
        speedread-font-size-scale-factor 2.0
        ;; speedread-text-justification =
        ;; speedread-end-sentence-regexp
        )
  )



;; 'spray-mode, 'speedread
(global-set-key (kbd "<f9>") 'spray-mode)


(provide 'init-my-tool-OpenSpritz)

;;; init-my-tool-OpenSpritz.el ends here
