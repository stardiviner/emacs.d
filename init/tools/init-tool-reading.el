;;; init-tool-reading.el --- init reading for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; - https://www.gnu.org/philosophy/right-to-read.html

;;; Code:
;;===============================================================================
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
  :custom ((spray-wpm 250) ; words per minute
           (spray-height 400) ; Height of characters
           (spray-margin-top 2) ; Character margin at top of buffer. Characters are as big as spray text characters.
           (spray-margin-left 4)
           (spray-ramp 2) ; Initial words before ramping up to full speed.
           (spray-save-point t))
  :custom-face
  (spray-base-face ((t (:inherit 'default :foreground "black" :background "white" :family "DejaVu Sans"))))
  (spray-accent-face ((t (:inherit 'spray-base-face :foreground "red")))))

;;; [ amread-mode ] -- a minor mode helping user speed-reading. Similar with Spritz (speed read).

(use-package amread-mode
  :ensure t
  :bind (:map tools-prefix ("r" . amread-mode))
  :commands (amread-mode))

(provide 'init-tool-reading)

;;; init-tool-reading.el ends here
