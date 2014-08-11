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


;;; [ open and switch to buffer ]

(defun my-func/open-and-switch-to-buffer (the-command the-buffer-name &optional whether-switch-to-buffer)
  "Open a `COMMAND', and switch to that `BUFFER' depend on `OPTION'.

Usage: 

 (define-key my-org-prefix-map (kbd 'o')
   (lambda ()
     (interactive)
     (my-func/open-and-switch-to-buffer 'org-agenda-list '*Org Agenda*' t)))
"
  (interactive)
  (if (get-buffer the-buffer-name)
      (switch-to-buffer the-buffer-name)
    (funcall the-command)
    (bury-buffer)
    (when whether-switch-to-buffer
      (switch-to-buffer the-buffer-name))))


;;; file

;;; edit file as root

;;; Option A
(defun sudo-edit (&optional arg)
  "edit currently visited file as root.

with a prefix arg prompt for a file to visit.
will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         ;; TODO: don't use ido here.?
                         (ido-read-file-name "find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; (global-set-key (kbd "C-x C-r") 'sudo-edit)

;;; Option B: better
;;
;; Lately I’ve decided that such a command is a bit of an overhead, since we can
;; check the file permissions automatically anyways. While I’m not quite fond of
;; advising commands (debugging advised commands is no fun) this was an
;; excellent opportunity to exploit them (for great good):

;; FIXME: the tramp seems does not work correctly here.
(defadvice find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(if (featurep 'ido)
    (defadvice ido-find-file (after find-file-sudo activate)
      "Find file as root if necessary."
      (unless (and buffer-file-name
                   (file-writable-p buffer-file-name))
        (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))))




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
