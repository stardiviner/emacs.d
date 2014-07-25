;;; init-my-prog-project.el --- init Project settings for Emacs

;;; Commentary:


;;; Code:

;;; [ Projectile ]
;;
;; - [C-c p C-h] -- get Projectile key bindings help.
;; - [C-c p] -- prefix
;; - [C-c p h] -- helm-projectile
;; - [M-x projectile-.*] -- projectile commands.
;;
;;; Synopsis
;; Projectile is a project interaction library for Emacs. Its goal is to provide
;; a nice set of features operating on a project level without introducing
;; external dependencies(when feasible). For instance - finding project files
;; has a portable implementation written in pure Emacs Lisp without the use of
;; GNU find(but for performance sake an indexing mechanism backed by external
;; commands exists as well).
;;
;; Projectile tries to be practical - portability is great, but if some external
;; tools could speed up some task substantially and the tools are available,
;; Projectile will leverage them.
;;
;; This library provides easy project management and navigation. The concept of
;; a project is pretty basic - just a folder containing special file. Currently
;; git, mercurial, darcs and bazaar repos are considered projects by default. So
;; are lein, maven, sbt, rebar and bundler projects. If you want to mark a
;; folder manually as a project just create an empty .projectile file in
;; it. Some of Projectile's features:
;;
;; - jump to a file in project
;; - jump to a directory in project
;; - jump to a project buffer
;; - jump to a test in project
;; - toggle between code and its test
;; - jump to recently visited files in the project
;; - switch between projects you have worked on
;; - kill all project buffers
;; - replace in project
;; - multi-occur in project buffers
;; - grep in project
;; - regenerate project etags
;; - visit project in dired
;; - run make in a project with a single key chord

(require 'projectile)


(setq projectile-keymap-prefix (kbd "C-c p"))

;; Indexing method
(setq projectile-use-native-indexing t)
;; Caching
(setq projectile-enable-caching t)
;; Using Projectile everywhere
;; If you want Projectile to be usable in every directory (even without the presence of project file):
(setq projectile-require-project-root nil)
;; Completion Options
(setq projectile-completion-system 'default) ; 'ido, 'grizzl, 'default

(setq projectile-tags-command "ctags -Re %s"
      projectile-use-git-grep nil)

(projectile-global-mode)
;; OR
;; (dolist (hook
;;          '(prog-mode-hook
;;            ))
;;   (add-hook hook 'projectile-on))



;;; Define Projects

;;; Manually create an empty file `.projectile' under that folder which you want
;;; make it to be a project root.
;;; $ touch dir-root/.projectile


;; Helm Integration
(if (featurep 'helm)
    (global-set-key (kbd "C-c p h") 'helm-projectile))





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
      pe/width 20
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


;;; [ vagrant ]

;; This package lets you send vagrant commands while working within a
;; project containing a Vagrantfile.
;;
;; It will traverse the directory tree until a Vagrantfile is found
;; and assume this is the box you want to work with. It can be handy
;; to bring a box up, (re)provision, or even ssh to without leaving
;; emacs.
;;
;; The emacs command `vagrant-up` will run `vagrant up` in a shell,
;; other commands follow the pattern `vagrant-X` emacs command runs
;; `vagrant X` in the shell. An exception is vagrant-edit, which will
;; open the Vagrantfile for editing.

;; - [M-x vagrant-up]
;; - [M-x vagrant-X] :: X is the pattern vagrant [X] action.

;; (require 'vagrant)




(provide 'init-my-prog-project)

;;; init-my-prog-project.el ends here
