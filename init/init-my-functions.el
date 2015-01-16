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


;;; [ defadvice + macro ]

;;; Advise Multiple Commands in the Same Manner.

(defmacro advice-commands (advice-name commands &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS. The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (before ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))

;;; usage examples:

;;; redundant way,
;; (defadvice switch-to-buffer
;;     (before switch-to-buffer-auto-save activate)
;;   (prelude-auto-save))
;; (defadvice other-window
;;     (before other-window-auto-save activate)
;;   (prelude-auto-save))

;;; reduce code way,
;; advice all window switching functions
;; (advice-commands "auto-save"
;;                  (switch-to-buffer other-window windmove-up windmove-down windmove-left windmove-right)
;;                  (save-buffer)
;;                  ;; (prelude-auto-save)
;;                  )

;;; `macroexpand' can show us how the macro gets expanded:
;; (macroexpand '(advice-commands "auto-save"
;;                                (switch-to-buffer other-window windmove-up windmove-down windmove-left windmove-right)
;;                                (prelude-auto-save)))
;;
;; (progn
;;   (defadvice switch-to-buffer
;;       (before switch-to-buffer-auto-save activate) (prelude-auto-save))
;;   (defadvice other-window
;;       (before other-window-auto-save activate) (prelude-auto-save)))



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
;; (defun sudo-edit (&optional arg)
;;   "edit currently visited file as root.

;; with a prefix arg prompt for a file to visit.
;; will also prompt for a file to visit if current
;; buffer is not visiting a file."
;;   (interactive "p")
;;   (if (or arg (not buffer-file-name))
;;       (find-file (concat "/sudo:root@localhost:"
;;                          ;; TODO: don't use ido here.?
;;                          (ido-read-file-name "find file(as root): ")))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
;;
;; ;; (global-set-key (kbd "C-x C-r") 'sudo-edit)
;;
;; ;;; Option B: better
;; ;;
;; ;; Lately I’ve decided that such a command is a bit of an overhead, since we can
;; ;; check the file permissions automatically anyways. While I’m not quite fond of
;; ;; advising commands (debugging adviced commands is no fun) this was an
;; ;; excellent opportunity to exploit them (for great good):
;;
;; FIXME: the tramp seems does not work correctly here.
;; (defadvice find-file (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
;;
;; (if (featurep 'ido)
;;     (defadvice ido-find-file (after find-file-sudo activate)
;;       "Find file as root if necessary."
;;       (unless (and buffer-file-name
;;                    (file-writable-p buffer-file-name))
;;         (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))))


;;; [ keybindings ]

;;; keybinding lookup
(defun bunch-of-keybinds (key)
  "Look up where is the KEY in key-maps."
  (interactive)
  (list
   (minor-mode-key-binding key)
   (local-key-binding key)
   (global-key-binding key)
   (overlay-key-binding key)
   )
  )

(defun overlay-key-binding (key)
  "Look up KEY in which key-map."
  (mapcar (lambda (keymap) (lookup-key keymap key))
          (cl-remove-if-not
           #'keymapp
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'keymap))
                   (overlays-at (point))))))

;;; Keys can be bound in 4 ways. By order of precedence, they are:
;;;
;;;     at point (overlays or text-propeties),
;;;     in minor-modes,
;;;     in buffers (where major-mode or buffer-local keybinds go),
;;;     and globally.
;;;
;;; The following function queries each one of these possibilities, and returns or prints the result.
;;
;; (defun locate-key-binding (key)
;;   "Determine in which keymap KEY is defined."
;;   (interactive "kPress key: ")
;;   (let ((ret
;;          (list
;;           (key-binding-at-point key)
;;           (minor-mode-key-binding key)
;;           (local-key-binding key)
;;           (global-key-binding key))))
;;     (when (called-interactively-p 'any)
;;       (message "At Point: %s\nMinor-mode: %s\nLocal: %s\nGlobal: %s"
;;                (or (nth 0 ret) "") 
;;                (or (mapconcat (lambda (x) (format "%s: %s" (car x) (cdr x)))
;;                               (nth 1 ret) "\n             ")
;;                    "")
;;                (or (nth 2 ret) "")
;;                (or (nth 3 ret) "")))
;;     ret))

;;; improved version function:
(defun key-binding-at-point (key)
  "Lookup the KEY at point in which key-map."
  (mapcar (lambda (keymap) (lookup-key keymap key))
          (cl-remove-if-not
           #'keymapp
           (append
            (mapcar (lambda (overlay)
                      (overlay-get overlay 'keymap))
                    (overlays-at (point)))
            (get-text-property (point) 'keymap)
            (get-text-property (point) 'local-map)))))


;;; require and install

;;; this will lead company-mode complete string from (require into (rrequire.
;; (defun require-x (feature &optional filename noerror package refresh)
;;   "A replacement for `require' which also installs the feature if it is absent.
;; - If FEATURE is present, `require' it and return t.
;;
;; - If FEATURE is not present, install PACKAGE with `package-install'.
;; If PACKAGE is nil, assume FEATURE is the package name.
;; After installation, `require' FEATURE.
;;
;; FILENAME is passed to `require'.
;;
;; If NOERROR is non-nil, don't complain if the feature couldn't be
;; installed, just return nil.
;;
;; By default, the current package database (stored in
;; `package-archive-contents') is only updated if it is empty.
;; Passing a non-nil REFRESH argument forces this update."
;;   (or (require feature filename t)
;;       (let ((package (or package
;;                          (if (stringp feature)
;;                              (intern feature)
;;                            feature))))
;;         (require 'package)
;;         (unless (and package-archive-contents (null refresh))
;;           (package-refresh-contents))
;;         (and (condition-case e
;;                  (package-install package)
;;                (error (if noerror nil (error (cadr e)))))
;;              (require feature filename noerror)))))


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
