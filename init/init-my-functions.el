;;; init-my-functions.el --- my functions collections

;;; Commentary:

;;; use `my-func' as prefix for every function.

;;; Code:

;;; [ Macros ]

;;; Evaluate elisp after feature has loaded.
(defmacro after (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))


;;; keybindings

;;; Usage: (local-set-minor-mode-key '<minor-mode> (kbd "key-to-hide") nil).
(defun local-set-minor-mode-key (mode key def)
  "Overrides a minor MODE keybinding KEY (definition DEF) for the local buffer.
by creating or altering keymaps stored in buffer-local
`minor-mode-overriding-map-alist'."
  (let* ((oldmap (cdr (assoc mode minor-mode-map-alist)))
         (newmap (or (cdr (assoc mode minor-mode-overriding-map-alist))
                     (let ((map (make-sparse-keymap)))
                       (set-keymap-parent map oldmap)
                       (push `(,mode . ,map) minor-mode-overriding-map-alist) 
                       map))))
    (define-key newmap key def)))



;;; notify (notify-send, )

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
