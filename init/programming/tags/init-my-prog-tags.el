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


;;; [ tags settings ]

(setq 
 ;; List of file names of tags tables to search.
 tags-add-tables t ; always add new tags to tables
 ;; tags-included-tables
 ;; tags-table-list (list
 ;;                  (expand-file-name "/usr/share/lib/TAGS"))
 tags-revert-without-query t
 ;; tags-completion-table
 tags-apropos-verbose t
 )


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



;;; [ cscope ] -- This is an interface from GNUemacs to Joe Steffen's "cscope" C browser.

;; Usage:
;; $ cscope -bR
;; - `cscope-minor-mode' ::

(require 'cscope)

(setq cscope-bindings-2deep nil
      cscope-bindings-3deep t
      cscope-blurb nil
      )



;; TODO:
;; (defun my-find-tag (&optional prefix)
;;   "union of `find-tag' alternatives. decides upon major-mode"
;;   (interactive "P")
;;   (if (and (boundp 'cscope-minor-mode)
;;            cscope-minor-mode)
;;       (progn
;;         (ring-insert find-tag-marker-ring (point-marker))
;;         (call-interactively
;;          (if prefix
;;              'cscope-find-this-symbol
;;            'cscope-find-global-definition-no-prompting
;;            )))
;;     (call-interactively 'find-tag)))
;;
;; (substitute-key-definition 'find-tag 'my-find-tag  global-map)


;;; [ xcscope ] -- interface of cscope.

;; (require 'xcscope)


;;; [ ascope ] -- another interface of cscope.

;;; ascope is an improvement over xcscope that runs all queries through a single
;;; cscope process, instead of starting a new process and reloading the database
;;; for each query.

;; Usage:
;;load this script using (require 'ascope.el) in you .emacs
;; M-x ascope-init load the cscope database. This command must be issue prior to issue any other command below, the directory feed to this command must be the directory include the cscope.out file
;; M-x ascope-find-global-definition
;; M-x ascope-find-this-symbol
;; M-x ascope-find-this-text-string
;; M-x ascope-find-functions-calling-this-function
;; M-x ascope-find-called-functions
;; M-x ascope-find-files-including-file
;; M-x ascope-all-symbol-assignments
;; M-x ascope-clear-overlay-arrow
;; M-x ascope-pop-mark
;;
;;run next commands in the search result buffer (*Result)
;;ascope-next-symbol this command is bind to key "n"
;;ascope-prev-symbol this command is bind to key "p"
;;ascope-select-entry-other-window-delete-window this command is bind to key "enter"

(require 'ascope)


;;; [ rscope ]

;;; rscope is a new implementation taking its roots from ascope, thus running a
;;; single cscope process for each cscope database. It’s a bit more versatile
;;; than ascope because it copes with multiple cscope databases (and hence
;;; spawns one cscope process per database once).


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


;;; [ helm-cscope ] -- cscope with Helm interface.

;;; Usage:
;;
;; interactive functions
;;
;;     - helm-cscope-find-symbol
;;     - helm-cscope-find-global-definition
;;     - helm-cscope-find-called-function
;;     - helm-cscope-find-calling-this-funtcion
;;     - helm-cscope-select (uses all of above sources)

(require 'helm-cscope)

(dolist (hook '(prog-mode-hook
                c-mode-hook
                c++-mode-hook
                ))
  (add-hook hook 'helm-cscope-mode))

;; ;;; set key-bindings
;; ;; 1. better then 2.
;; (add-hook 'helm-cscope-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd (concat my-prog-help-document-map " M-t")) 'helm-cscope-find-symbol)))


;; ;; 2.
;; (eval-after-load "helm-cscope"
;;   '(progn
;;      ;; use "C-h d l" as prefix. so use "C-h d" prefix map.
;;      (define-key helm-cscope-mode-map (kbd (concat my-prog-help-document-map "M-t"))
;;        'helm-cscope-find-symbol)
;;      (define-key helm-cscope-mode-map (kbd (concat my-prog-help-document-map "M-r"))
;;        'helm-cscope-find-global-definition)
;;      (define-key helm-cscope-mode-map (kbd (concat my-prog-help-document-map "M-g M-c"))
;;        'helm-cscope-find-called-function)
;;      (define-key helm-cscope-mode-map (kbd (concat my-prog-help-document-map "M-g M-p"))
;;        'helm-cscope-find-calling-this-funtcion)
;;      (define-key helm-cscope-mode-map (kbd (concat my-prog-help-document-map "M-s"))
;;        'helm-cscope-select)
;;      ))

;; (define-key global-map (kbd (concat my-prog-help-document-map "C-h d M-t")) 'helm-cscope-find-symbol)


;;; [ ido-find-tag ] --- custom function.

;;; TODO:
;; - whether current point word has tag in TAGS file. press [M-.]
;; - if have, then jump directly.
;; - if not, then open ido-find-tag.
;; - let ido-find-tag auto integrate with projectile etc tool to find project root to locate TAGS file.
;; - if not find, then ask user: manually set? or generate with root path, and ctags command arguments.

;; (require 'etags)

;; (defun ido-find-tag ()
;;   "Find a tag using ido"
;;   (interactive)
;;   (tags-completion-table)
;;   (let (tag-names)
;;     (mapc (lambda (x)
;;         (unless (integerp x)
;;           (push (prin1-to-string x t) tag-names)))
;;       tags-completion-table)
;;     (find-tag (ido-completing-read "Tag: " tag-names))))
 
;; (defun ido-find-file-in-tag-files ()
;;   (interactive)
;;   (save-excursion
;;     (let ((enable-recursive-minibuffers t))
;;       (visit-tags-table-buffer))
;;     (find-file
;;      (expand-file-name
;;       (ido-completing-read
;;        "Project file: " (tags-table-files) nil t)))))
 
;; (global-set-key [remap find-tag] 'ido-find-tag)
;; (global-set-key (kbd "M-.") 'ido-find-file-in-tag-files)



;;; Select Tags

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


;;; [ gtags ] -- (global)




;;; [ helm-gtags ]

; (require ''setup-helm-gtags)



(provide 'init-my-prog-tags)

;;; init-my-prog-tags.el ends here
