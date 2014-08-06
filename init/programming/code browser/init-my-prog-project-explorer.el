;;; init-my-prog-project-explorer.el --- 
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ project-explorer ] -- A project explorer sidebar.

;;; Features:
;;
;; * Asynchronous indexing
;; * Caching
;; * File management
;; * I-search support
;; * Helm support
;; * Folding
;; * Filtering

;;; Usage:
;;; Main Commands:
;; - project-explorer-open -- open the sidebar
;; - project-explorer-helm -- browse the file collection using helm
;;
;;; Main key-bindings:
;; - "s"        Change directory
;; - "j"        Next line
;; - "k"        Previous line
;; - "g"        Refresh
;; - "+"        Create file or directory (if the name ends with a slash)
;; - "-" & "d"  Delete file or directory
;; - "c"        Copy file or directory
;; - "r"        Rename file or directory
;; - "q"        Hide sidebar
;; - "u"        Go to parent directory
;; - "["        Previous sibling
;; - "]"        Next sibling
;; - "TAB"      Toggle folding. Unfold descendants with C-U
;; - "S-TAB"    Fold all. Unfold with C-U
;; - "RET"      Toggle folding of visit file. Specify window with C-U
;; - "f"        Visit file or directory. Specify window with C-U
;; - "w"        Show the path of file at point, and copy it to clipboard
;; - "M-k"      Launch ack-and-a-half, from the closest directory
;; - "M-l"      Filter using a regular expression. Call with C-u to disable
;; - "M-o"      Toggle omission of hidden and temporary files

(setq pe/side 'left
      pe/width 30
      ;; pe/project-root-function
      ;; pe/directory-tree-function
      ;; pe/cache-enabled
      ;; pe/omit-regexp
      )

;;; ---------------------------------------------------------------------
;; Tailor-fitting pe/project-root-function
;;
;; The default pe/project-root-function assumes that the project root will be
;; the closest directory containing .git, or the current directory. If you have
;; projectile installed, it will simply use projectile-project-root
;; instead. Should you need something more specific, it's not hard to make your
;; own project-finding function.
;;
;; (defun pe/project-root-function-sample ()
;;   (expand-file-name
;;    (or
;;     ;; A specific directory
;;     (when (string-prefix-p "/path/to/my/project/" default-directory)
;;       "/path/to/my/project/")
;;     ;; A directory containg a file
;;     (locate-dominating-file default-directory "Web.config")
;;     default-directory)))
;;
;; (setq pe/project-root-function 'pe/project-root-function-sample)
;;; ---------------------------------------------------------------------




(provide 'init-my-prog-project-explorer)

;;; init-my-prog-project-explorer.el ends here
