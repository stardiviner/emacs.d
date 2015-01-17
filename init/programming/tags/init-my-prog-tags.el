;;; init-my-prog-tags.el -- init tags file config for Emacs.

;;; Commentary:

;;; Code:


(unless (boundp 'my-prog-lookup-tags-map)
  (define-prefix-command 'my-prog-lookup-tags-map))
(define-key my-prog-lookup-map (kbd "t") 'my-prog-lookup-tags-map)


;;; [ tags settings ]

(setq tags-add-tables t ; always add new tags to tables
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
;; TODO
;; (setq my-tags-file-location (concat (projectile-project-p) "TAGS"))


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


;;; [ Imenu ]

;;; Imenu produces menus for accessing locations in documents, typically in the
;;; current buffer. You can access the locations using an ordinary menu (menu
;;; bar or other) or using minibuffer completion.

;;; [ imenu-anywhere ]

;;; imenu-anywhere command pops an IDO interface with all the imenu tags across
;;; all buffers with the same mode as the current one. In a sense it is similar
;;; to etag selection, but works only for the open buffers. This is often more
;;; convenient as you don't have to explicitly build the etags table.

(if (featurep 'imenu-anywhere)
    (if (featurep 'helm)
        (global-set-key (kbd "C-.") 'helm-imenu-anywhere)
      (global-set-key (kbd "C-.") 'imenu-anywhere)))




(require 'init-my-prog-tags-etags)
(require 'init-my-prog-tags-ctags)
(require 'init-my-prog-tags-gtags)



(provide 'init-my-prog-tags)

;;; init-my-prog-tags.el ends here
