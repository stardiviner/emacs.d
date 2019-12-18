;;; init-tool-reading.el --- init reading for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; - https://www.gnu.org/philosophy/right-to-read.html

;;; Code:

;;; [ idle-scroll ] -- idle scroll buffer one line every second.

(use-package idle-scroll
  :ensure t
  :defer t
  :commands (idle-scroll-mode)
  :bind (:map tools-prefix ("M-r" . idle-scroll-mode)))

;;; [ novel reading mode ]

(defun novel-read-mode ()
  "Setup current frame to be suitable for reading long novel/article text.

• Set frame width to 70
• Line wrap at word boundaries.
• Line spacing is increased.
• Proportional width font is used.

Call again to toggle back.
URL `http://ergoemacs.org/emacs/emacs_novel_reading_mode.html'
Version: 2019-01-30
Author: xah"
  (interactive)
  (let ()
    (if (eq (frame-parameter (selected-frame) 'width) 70)
        (progn
          (set-frame-parameter (selected-frame) 'width 106)
          (variable-pitch-mode 0)
          (setq line-spacing nil)
          (setq word-wrap nil))
      (progn
        (set-frame-parameter (selected-frame) 'width 70)
        (variable-pitch-mode 1)
        (setq line-spacing 0.4)
        (setq word-wrap t))))
  (redraw-frame (selected-frame)))

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

;;; [ greader ] -- gnamù reader, a reader with Espeak TTS.

(use-package greader
  :ensure t
  :commands (greader-mode)
  :bind (:map tools-prefix ("r" . greader-mode))
  ;; disable show shell command process output to minibuffer.
  :init (setq greader-filter-enabled nil))


(provide 'init-tool-reading)

;;; init-tool-reading.el ends here
