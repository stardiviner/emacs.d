;;; init-my-functions.el --- my functions collections

;;; Commentary:

;;; use `my-func' as prefix for every function.

;;; Code:

;;; notify(notify-send, )

(defun my-func-notify-send (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise;

  TITLE is the title of the message, MSG is the context, ICON is
  the icon for notify-send, SOUND is the audio file.  Optionally,
  you can provide an ICON and a sound to be played."
  (interactive)
  (when sound (shell-command
               (concat "mplayer -really-quiet " sound " 2> /dev/null")))
  (if (eq window-system 'x)
      (shell-command (concat "notify-send "
                             (if icon (concat "-i " icon) "")
                             " '" title "' '" msg "'"))
    ;; text only version
    (message (concat title ": " msg))))

;;; e.g.
;; (my-func-notify-send "Warning" "the end is near" "/usr/share/icons/test.png" "/usr/share/sounds/beep.ogg")



(provide 'init-my-functions)

;;; init-my-functions.el ends here
