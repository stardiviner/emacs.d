;;; init-tool-OpenSpritz.el --- init Spray for Emacs OpenSpritz implement.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; - https://www.gnu.org/philosophy/right-to-read.html

;;; Code:

;;; [ spray ] -- [Emacs] an elisp implementation of OpenSpritz

(use-package spray
  :ensure t
  :commands (spray-mode)
  :bind ("<f9>" . spray-mode)
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


(provide 'init-tool-OpenSpritz)

;;; init-tool-OpenSpritz.el ends here
