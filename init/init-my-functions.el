;;; init-my-functions.el --- my functions collections

;;; Commentary:

;;; use `my-func' as prefix for every function.

;;; Code:

;;; Group hooks into one new hook.

(defmacro hook-modes (modes &rest body)
  (declare (indent 1))
  `(--each ,modes
     (add-hook (intern (format "%s-hook" it))
               (lambda () ,@body))))

;;; Usage:
;;
;; (defvar progish-modes
;;   '(prog-mode css-mode sgml-mode))
;;
;; (hook-modes progish-modes
;;             (highlight-symbol-mode)
;;             (highlight-symbol-nav-mode))
;; instead of:
;; (hook-modes progish-modes 'highlight-symbol-mode)

;;; It is same as:
;; (defun my-non-special-mode-setup ()
;;   (setq show-trailing-whitespace t)
;;   ...)
;; (dolist (hook '(prog-mode-hook text-mode-hook css-mode-hook ...))
;;   (add-hook hook 'my-non-special-mode-setup))


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

;;; magical insert kbd for key.
(defun insert-kbd-for-key (key)
  (interactive "kKey: ")
  (insert (format "(kbd \"%s\")" (key-description key))))

;; (global-set-key "C-c k" 'insert-kbd-for-key)


;;; [ open and switch to buffer ]

(defun my-func/open-and-switch-to-buffer (the-command the-buffer-name &optional whether-switch-to-buffer)
  "Open a `COMMAND', and switch to that `BUFFER' depend on `OPTION'.

Usage: 

 (define-key my-org-prefix (kbd 'o')
   (lambda ()
     (interactive)
     (my-func/open-and-switch-to-buffer 'org-agenda-list \"*Org Agenda*\" t)))
"
  (interactive)
  (if (get-buffer the-buffer-name)
      (switch-to-buffer the-buffer-name)
    (funcall the-command)
    (bury-buffer)
    (when whether-switch-to-buffer
      (switch-to-buffer the-buffer-name))))


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


;;; [ align ]

(defun align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun align-& (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))




;;; read in encrypted JSON file key-value pairs.

(setq my/account-file (concat user-emacs-directory "accounts.json.gpg"))

(require 'json)

(defun my/json-read-value (file key)
  "Read in JSON `FILE' and get the value of symbol `KEY'."
  (cdr (assoc key
              (json-read-file (if (file-exists-p my/account-file)
                                  my/account-file
                                (concat user-emacs-directory "accounts.json.gpg"))))))

;; ask for GPG password at first, not in middle of Emacs startup progress.
(my/json-read-value my/account-file 'yagist)



(provide 'init-my-functions)

;;; init-my-functions.el ends here
