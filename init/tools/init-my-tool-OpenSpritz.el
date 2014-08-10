;;; init-my-tool-OpenSpritz.el --- init Spray for Emacs OpenSpritz implement.
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; - https://www.gnu.org/philosophy/right-to-read.html

;;; Code:

;;; [ Spray ] -- [Emacs] an elisp implementation of OpenSpritz

;;; Usage:
;;
;; Commands
;; In spray-mode buffers, following commands are available.
;;
;; spray-start/stop (SPC)
;;     pause or resume spraying
;; spray-backward-word (h, )
;;     pause and back to the last word
;; spray-forward-word (l, )
;;     inverse of spray-backward-word 
;; spray-faster (f)
;;     increases speed
;; spray-slower (s)
;;     decreases speed
;; spray-quit (q, )
;;     quit spray-mode 

(require 'spray)

(setq spray-wpm 400 ; words per minute
      spray-height 400 ; Height of characters
      spray-margin-top 5 ; Character margin at top of buffer. Characters are as big as spray text characters.
      spray-margin-left 4
      spray-ramp 2 ; Initial words before ramping up to full speed.
      )

(set-face-attribute 'spray-base-face nil
                    )
(set-face-attribute 'spray-accent-face nil
                    :foreground "red"
                    )

;; (define-key spray-mode-map (kbd ""))


;;; [ speedread ] -- Aid to speedreading emacs buffers

;;; Usage:
;;
;;; Concept: load a (text) file into a buffer for speedreading.
;;
;; Position the cursor to the point at which you wish to start reading and give the command.
;;
;; [M-x speedread]
;;
;; The file will be displayed to you in the echo area (!) a bit at a time, in
;; “flashes” with a delay between each flash. The customization variables
;; control the minimum size (in characters) of each flash group, and the pause
;; between groups.
;;
;; After a certain number of groups have been displayed (there is a
;; customization variable for this too) there is a ‘hard’ pause. This is quite
;; necessary to avoid incredible eye fatigue!
;;
;; Rather than waiting for a pause between flashes, if you like, you can alter
;; the speed, the flash group size, or the number of flashes on-the-fly at just
;; about any time.
;;
;; When starting a speedread session, if a bookmark exists you are asked if you
;; wish to use it. If you choose not to use it, the display starts at the
;; current cursor position.
;;
;; Newline characters are converted to spaces. This causes a little weirdness at
;; times but leaving newlines intact makes a big mess.
;;
;; Again, tune the display parameters! You may find that as your speed-reading
;; skills improve you can increase the number of characters in a flash group,
;; and/or decrease the pause time between groups.
;;
;; Project Gutenberg is a fabulous source of texts to use.
;;
;;; Tuning:
;;
;; Since speed is very much system dependent these params MUST be fine-tuned to
;; get any sort of acceptable results. Advice: do it on-the-fly and then when
;; you get what you like save as permanent customized values. Set the size of
;; the flash group and the delay between groups.
;;
;;
;; Keybindings
;;
;;     ‘f’ : to go 20% faster,
;;     ’s’ : to go 20% slower,
;;     ‘w’ : to widen the flash group 20%,
;;     ‘n’ : to narrow it 20%,
;;     ‘m’ : for 20% more flashes between pauses,
;;     ‘l’ : for 20% less flashes between pauses,
;;     ‘b’ : to go back and repeat the current set of flashes,
;;     ‘r’ : to completely restart from whatever point in the buffer you began the session.
;;     ‘q’ : to quit and save the bookmark at point;
;;     ‘e’ : to exit without saving the bookmark.
;;
;; Commands
;;
;; Below are complete command list:
;;
;;     `speedread’ : Speedread a buffer by timed flashing of groups of words in the echo area
;;     `speedread-faster’ : Increase reading speed temporarily
;;     `speedread-narrower’ : Narrow flash-group temporarily
;;     `speedread-help’ : Get speedreading command help
;;     `speedread-save-changes’ : save all customization variables changed this Emacs session
;;
;; Comments
;;
;; The parameters ‘out of the box’ tend to result in roughly 600 words per
;; minute. This is probably too fast for many people; adjust to suit. Don’t
;; attempt too much speed initially or you will become very frustrated and
;; probably give up. As you learn how to work with the technique you can build
;; up the speed and the flash group size. On the other hand, push yourself a
;; little. Go as fast as you can without losing comprehension. Different types
;; of reading material will require different speed settings! You can read a
;; scifi novel faster than you can read existential philosophy.

;; (require 'speedread)

;; (setq speedread-chars 20 ; Minimum characters per flash group
;;       speedread-delay-milliseconds 300 ; Milliseconds of delay between flashes
;;       speedread-end-sentence-delay-milliseconds 500 ; Pause between sentences in milliseconds
;;       speedread-final-delay-milliseconds 2000 ; Milliseconds to wait before exiting after reaching end of buffer
;;       speedread-top-window-size 5 ; size of frame to use for displaying the original buffer whilst speedreading.
;;       speedread-font-size-scale-factor 2.0 ; Scale factor for size of font
;;       ;; speedread-text-justification = ; Justification of text in speedread buffer. default = (quote center)
;;       ;; speedread-end-sentence-regexp  ; Regular expression to match with end of sentence words.
;;       )



;; 'spray-mode, 'speedread
(global-set-key (kbd "<f6>") 'spray-mode)


(provide 'init-my-tool-OpenSpritz)

;;; init-my-tool-OpenSpritz.el ends here
