;;; init-my-org-obsolete.el --- init for My old org config
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;; Applying Markup to Strings in org-mode.
;; Normally, org-mode ignores your attempts to markup text that starts with " or '.
;; This HAS to come before (require 'org)
;; (setq org-emphasis-regexp-components
;;       '("     ('\"{“”"
;;         "-   .,!?;''“”\")}/\\“”"
;;         "    \r\n,"
;;         "."
;;         1))


;;; [ Org Modules ]
;; Modules that should always be loaded together with org.el.
(setq org-modules '(org-pcomplete
                    org-faces
                    ;; org-fstree
                    org-table org-compat
                    ;; org-protocol
                    org-timer org-clock org-habit org-notify
                    org-info org-bibtex org-docview
                    org-plot
                    org-bbdb
                    org-irc ; org-gnus org-mhe org-rmail
                    ;; org-w3m
                    ))


(add-hook 'org-mode-hook
          (lambda ()
            (when (org-bullets-mode)
              (org-indent-mode 1))
            ))



;;;_* org-annotate-file

;; This is yet another implementation to allow the annotation of a
;; file without modification of the file itself. The annotation is in
;; org syntax so you can use all of the org features you are used to.

;; (require 'org-annotate-file)

;; (global-set-key (kbd "C-c C-l") 'org-annotate-file)

;; ;; To change the location of the annotation file:
;; (setq org-annotate-file-storage-file "~/.emacs.d/.org-annotated.org")

;; ;; Then when you visit any file and hit C-c C-l you will find yourself
;; ;; in an org buffer on a headline which links to the file you were
;; ;; visiting, e.g:
;; ;; * ~/org-annotate-file.el

;; ;; Under here you can put anything you like, save the file
;; ;; and next time you hit C-c C-l you will hit those notes again.
;; ;;
;; ;; To put a subheading with a text search for the current line set
;; ;; `org-annotate-file-add-search` to non-nil value. Then when you hit
;; ;; C-c C-l (on the above line for example) you will get:
;; (setq org-annotate-file-add-search t)

;; * ~/org-annotate-file.el
;; ** `org-annotate-file-add-search` to non-nil value. Then whe...

;; Note that both of the above will be links.




;; (defun my-dnd-func (event)
;;   (interactive "e")
;;   (goto-char (nth 1 (event-start event)))
;;   (x-focus-frame nil)
;;   (let* ((payload (car (last event)))
;;          (type (car payload))
;;          (fname (cadr payload))
;;          (img-regexp "\\(png\\|jp[e]?g\\)\\>"))
;;     (cond
;;      ;; insert image link
;;      ((and  (eq 'drag-n-drop (car event))
;;           (eq 'file type)
;;           (string-match img-regexp fname))
;;       (insert (format "[[%s]]" fname))
;;       (org-display-inline-images t t))
;;      ;; insert image link with caption
;;      ((and  (eq 'C-drag-n-drop (car event))
;;           (eq 'file type)
;;           (string-match img-regexp fname))
;;       (insert "#+ATTR_ORG: :width 300\n")
;;       (insert (concat  "#+CAPTION: " (read-input "Caption: ") "\n"))
;;       (insert (format "[[%s]]" fname))
;;       (org-display-inline-images t t))
;;      ;; C-drag-n-drop to open a file
;;      ((and  (eq 'C-drag-n-drop (car event))
;;           (eq 'file type))
;;       (find-file fname))
;;      ((and (eq 'M-drag-n-drop (car event))
;;          (eq 'file type))
;;       (insert (format "[[attachfile:%s]]" fname)))
;;      ;; regular drag and drop on file
;;      ((eq 'file type)
;;       (insert (format "[[%s]]\n" fname)))
;;      (t
;;       (error "I am not equipped for dnd on %s" payload)))))
;;
;; (define-key org-mode-map (kbd "<drag-n-drop>") 'my-dnd-func)
;; (define-key org-mode-map (kbd "<C-drag-n-drop>") 'my-dnd-func)
;; (define-key org-mode-map (kbd "<M-drag-n-drop>") 'my-dnd-func)



;; 2. use `file-expand-wildcards'
;; (setq org-agenda-files (file-expand-wildcards "~/Org/*.org")) ; Including all org files from a directory into the agenda
;; 3. use `find-lisp-find-files'
;;; Sync with Dropbox
;; (autoload 'find-lisp-find-files "find-lisp")
;; (setq org-agenda-files
;;       (append (find-lisp-find-files "~/Sync/Dropbox" "\.org$") org-agenda-files))
;; (setq org-agenda-files (find-lisp-find-files "~/Org" "\.org$"))
;; (setq org-agenda-files
;;       (cl-remove-duplicates (append '("~/Org/GTD/"
;;                              "~/Org/Work/"
;;                              "~/Org/Capture/"
;;                              "~/Org/Projects/"
;;                              "~/Org/Wiki/Learning/My_Learning_Plan.org"
;;                              "~/Org/Wiki/Learning/Learn-English.org"
;;                              )
;;                            (find-lisp-find-files "~/Org" "\.org$"))))

;;; ISSUE and seems this (find-lisp-find-files) list in variable
;;; org-agenda-files can not update when already has new files added.

;; -2
;; recursively find .org files in provided directory
;; modified from an Emacs Lisp Intro example
;; (defun find-org-file-recursively (directory &optional filext)
;;   "Return .org and .org_archive files recursively from DIRECTORY.
;; If FILEXT is provided, return files with extension FILEXT instead."
;;   (interactive "DDirectory name: ")
;;   (let* (org-file-list
;;          (case-fold-search t) ; filesystems are case sensitive
;;          (fileregex (if filext (format "^[^.#].*\\.\\(%s$\\)" filext)
;;                       "^[^.#].*\\.\\(org$\\|org_archive$\\)"))
;;          (cur-dir-list (directory-files directory t "^[^.#].*"))) ; exclude .*
;;     ;; loop over directory listing
;;     (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
;;       (cond
;;        ((file-regular-p file-or-dir) ; regular files
;;         (if (string-match fileregex file-or-dir) ; org files
;;             (add-to-list 'org-file-list file-or-dir)))
;;        ((file-directory-p file-or-dir)
;;         (dolist (org-file (find-org-file-recursively file-or-dir filext)
;;                           org-file-list) ; add files found to result
;;           (add-to-list 'org-file-list org-file)
;;           )))
;;       )
;;     ))
;; 0
;; (split-string (shell-command-to-string "find ~/Org -name '*.org' -print0"))
;; or
;; (find-lisp-find-files "~/Org" "\\.org$")
;; 1
;; (setq org-agenda-files (directory-files "~/Org/" :full "^[^.]"))
;; 2
;; (setq org-agenda-files (list "~/Org/"))
;; 3
;; (eval-after-load 'org
;;   '(progn
;;      ;; add all org files in the org directory to the agenda.
;;      (mapcar
;;       (lambda (file)
;;         (add-to-list 'org-agenda-files file))
;;       (directory-file (expand-file-name "~/Org/") t "\\.org"))
;;      ))
;; 4
;; (setq org-agenda-files (file-expand-wildcards "~/Org/*.org"))
;; 5
;; (defvar my-org-directories
;;   (list
;;    "~/Org/"
;;    ))
;; (defun my-org-files ()
;;   (mappend '(lambda (directory)
;;               (directory-files directory t "\\.org$"))
;;            my-org-directories))
;; (setq org-agenda-files (my-org-files))
;; 6
;; (setq org-agenda-text-search-extra-files) ; [C-c a s]
;; (setq org-agenda-file-regexp "\\`[^.].*\\.org\\'")
;; Note that these files will only be searched for text search commands, In
;; fact, if the first element in the list is the symbol `agenda-archives', then
;; all archive files of all agenda files will be added to the search scope.
;; [C-c a s]
;; TODO: test this setting.
;; (setq org-agenda-text-search-extra-files '(agenda-archives "~/Org/Journal.org" "~/Org/Diary/"))




;;;_* Custom Functions

;;; Promote all items in subtree
;; This function will promote all items in a subtree. Since I use subtrees
;; primarily to organize projects, the function is somewhat unimaginatively
;; called my-org-un-project:
(defun stardiviner/org-prompt-all-items-in-subtree ()
  "Promote all items in subtree.

This function will promote all items in a subtree."
  (interactive)
  (org-map-entries 'org-do-promote "LEVEL>1" 'tree)
  (org-cycle t))

;;; Turn a heading into an Org link
(defun stardiviner/turn-word-into-org-mode-link ()
  "Replace word at point by an Org mode link."
  (interactive)
  (when (org-at-heading-p)
    (let ((hl-text (nth 4 (org-heading-components))))
      (unless (or (null hl-text)
                 (org-string-match-p "^[ \t]*:[^:]+:$" hl-text))
        (beginning-of-line)
        (search-forward hl-text (point-at-eol))
        (replace-string
         hl-text
         (format "[[file:%s.org][%s]]"
                 (org-link-escape hl-text)
                 (org-link-escape hl-text '((?\] . "%5D") (?\[ . "%5B"))))
         nil (- (point) (length hl-text)) (point))))))





(provide 'init-my-org-obsolete)

;;; init-my-org-obsolete.el ends here
