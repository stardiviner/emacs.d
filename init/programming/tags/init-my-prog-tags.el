;;; init-my-prog-tags.el -- init tags file config for Emacs.

;;; Commentary:


;;; Emacs Tags -- etags

;; Usage:
;; - [M-.] -- (find-tag)
;; - [C-u M-.] -- go to the next match.
;; - [M-*] -- (pop-tag-mark) -- jump back.
;; - [M-x tags-search] -- regexp-search through the source files indexed by a tags file (a bit like `grep').
;; - [M-x tags-query-replace] -- query-replace through the source files indexed by a tags file.
;; - [M-,] -- (tags-loop-continue) -- resume `tags-search' or `tags-query-replace'.
;; - [M-x tags-apropos] -- list all tags in a tags file that match a regexp.
;; - [M-x list-tags] -- list all tags defined in a source file.

;; - [$ find . | grep ".*\.\(hh\|hxx\|cc\)" | xargs etags -f TAGS]


;; Exuberant Ctags -- ctags

;; - Emacs tags for ctags with `ctags -e`.
;; - [$ ctags -r -R .]


;;; Build Tags

(autoload 'projectile-project-p "projectile")

(defun nonempty-string-p (x)
  (and (stringp x)
     (plusp (length x))))

;;; decide which location for TAGS file. (smart finding)
; TODO
; (setq my-tags-file-location (concat (projectile-project-p) "TAGS"))

(defun create-tags (dir-name)
  "Create TAGS file in DIR-NAME recursively.

For project directory with Projectile root."
  ;; (interactive "DDirectory: ")
  ;; TODO fix this interactive function.
  (interactive (if (nonempty-string-p (projectile-project-p))
                   (read-from-minibuffer (projectile-project-root))
                 "DDirectory: "
                ))
  (shell-command
   (format "ctags -f %s -e -R %s"
           my-tags-file-location (directory-file-name dir-name)))
  )

(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo `find-tag'.

   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                (not (ding))
                (y-or-n-p "Buffer is modified, save it? ")
                (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))


;;; Ctags

;; Find root (replace eproject-root): cd "$(git rev-parse –show-toplevel)"

(defun stardiviner/build-ctags ()
  (interactive)
  (message "building project tags")
  (let ((root (eproject-root)))
    (shell-command (concat "ctags -e -R --extra=+fq --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " root)))
  (visit-project-tags)
  (message "tags built successfully"))

(defun stardiviner/visit-project-tags ()
  (interactive)
  (let ((tags-file (concat (eproject-root) "TAGS")))
    (visit-tags-table tags-file)
    (message (concat "Loaded " tags-file))))



;; Select Tags
;; [ Icicles ] -- Emacs Tags Enhancements
;; (require 'icicles)
;; (icy-mode 1)
;; EtagsSelect
;; VTags
;; (helm-c-etags-select)
;; (helm-etags-plus)

;; Finding file *in* TAGS



;;; [ etags ]
(require 'etags)


;;; [ etags-update ] --- a Emacs global minor mode that updates your TAGS when saving a file.
;;; https://github.com/mattkeller/etags-update
;;; Usage:
;; -

;;;  Installing
;; Put etags-update.pl in your shell's PATH, making sure it is executable. For instance:
;; $ mv etags-update.pl ~/bin
;; $ chmod 755 ~/bin/etags-update.pl
;; To install the minor-mode, put the etags-update.el file in your load-path:
;; mv etags-update.el ~/elisp
;;
;;; Usage:
;; 1. [M-x visit-tags-table <your-tags-file>] :: first load your project's TAGS file.
;; 2. [M-x etags-update-mode] :: Then toggle the minor-mode on. The same command toggles the minor-mode off.

;; The minor-mode is global, so it will be enabled in all your buffers. The
;; string "etu" appears in the mode-line when etags-update is enabled.
;;
;; When you save a file that is already listed in your TAGS file, the TAGS file
;; will automatically be updated.
;;
;; When you save a file that is not listed in your TAGS file, etags-update can
;; add the file to your TAGS. The etu/append-file-action variable controls what
;; happens. The default value, 'prompt, asks if you want to add the
;; file. Etags-update remembers your choice for a file.

;; (load-file "~/.emacs.d/my-init/extensions/etags-update.el")
(require 'etags-update)

;;; `etu/append-file-action'
;; What action should be taken when a file not already in TAGS is saved?
(autoload 'projectile-project-p "projectile")

(defun my-etu-append-file-action-p (file)
  "Determine which value should be used for variable `etu/append-file-action' for current FILE."
  (cond
   ((and
     (nonempty-string-p (projectile-project-p))
     (file-exists-p my-tags-file-location)) ; for files which in Projectile.
    'add)
   ((string= (file-name-extension (buffer-file-name)) "org") ; for org files.
    nil)
   (t 'prompt) ; for others
   )
  )

(setq etu-append-file-action 'my-etu-append-file-action-p)



;;; [ etags-u.el ]

;;; Usage:
;; - [etags-u-update-tags-file] ::
;;   It uses `tags-file-name' variable. To change it, use M-x `visit-tags-table' command.
;; - [etags-u-mode] :: [C-x C-s]
;;   It also provides minor mode `etags-u-mode'. In this minor mode, C-x C-s run `etags-u-update-tags-file' automatically.
;;
;;; Usage example:
;; M-x visit-tags-table
;; M-x etags-u-mode
;; <at this point file is ALREADY added to TAGS file>
;; <make changes>
;; C-x C-s
;; <changes updated in TAGS file>

;; (load-file "~/.emacs.d/my-init/extensions/etags-u.el")
;; (require 'etags-u)

;;; minor mode
;; (etags-u-mode t)
;;
;; (dolist (hook '(prog-mode-hook
;;                 ))
;;   (add-hook hook '(lambda ()
;;                     (etags-u-mode t))))




(provide 'init-my-prog-tags)

;;; init-my-prog-tags.el ends here
