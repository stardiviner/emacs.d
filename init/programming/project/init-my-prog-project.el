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
;;
;;
;;; Storing project settings
;;
;; From project to project some things may differ even in same language -
;; different coding styles, separate auto-completion sources, etc. If you need
;; to set some variables according to selected project, you can use standard
;; Emacs feature called [Per-Directory Local Variables]
;; (http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html). To
;; use it you must create file named `.dir-locals.el` inside project
;; directory. This file must contain something like this:
;;
;; ```
;; ((nil . ((eval . (
;;  (setq secret-ftp-password "secret")
;;  (setq compile-command "make target-x")))))
;;  (c-mode . (c-file-style . "BSD"))
;;  )
;; ```


(require 'projectile)


(setq projectile-keymap-prefix (kbd "C-c p"))

;; Indexing method
(setq projectile-use-native-indexing t)
;; Caching: nil, (* 10 60) [10 minutes],
(setq projectile-enable-caching t
      projectile-file-exists-remote-cache-expire '(* 30 60) ; remote file exists cache expire to 10 minutes
      )
;; Using Projectile everywhere
;; If you want Projectile to be usable in every directory (even without the presence of project file):
(setq projectile-require-project-root t)
;; Completion Options
(setq projectile-completion-system 'helm ; 'helm, 'ivy, 'ido, 'grizzl, 'default
      projectile-use-git-grep t ; use `vc-git-grep'
      )

;; (setq projectile-tags-command "ctags -Re %s")

(setq projectile-mode-line '(:eval
                             (format "(%s)" ; " Projectile/>[%s]"
                                     (projectile-project-name))))

;;; TODO: use append mode to change those variables instead of setting them.
;; (setq
;;  projectile-globally-ignored-modes '("erc-mode" "help-mode" "completion-list-mode"
;;                                      "Buffer-menu-mode" "gnus-.*-mode" "occur-mode")
;;  projectile-globally-ignored-directories '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" "build")
;;  projectile-globally-ignored-files '("TAGS")
;;  projectile-globally-ignored-buffers '("TAGS.*" "\*.*\*")
;;  )


;; (setq helm-source-projectile-files-list)

;; https://github.com/bbatsov/projectile/commit/e8b7b54449c645c764c928d7d1cfa29113230f05
;; (projectile-register-project-type)

(projectile-global-mode +1)
;; OR
;; (dolist (hook
;;          '(prog-mode-hook
;;            ))
;;   (add-hook hook 'projectile-on))



;;; Define Projects

;;; Manually create an empty file `.projectile' under that folder which you want
;;; make it to be a project root.
;;; $ touch dir-root/.projectile


;;; [ redefine projectile keybindings ]

(unless (boundp 'my-projectile-keymap-prefix)
  (define-prefix-command 'my-projectile-keymap-prefix))
(global-set-key (kbd "C-c p") 'my-projectile-keymap-prefix)

;; commander
;; (define-key my-projectile-keymap-prefix (kbd "m") 'projectile-commander)
;; ;; TAGS
;; ;; (define-prefix-command 'TAGS-)
;; ;; (global-set-key (kbd "C-c p T") 'TAGS-)
;; ;; (define-key TAGS- (kbd "r") 'projectile-regenerate-tags)
;; (define-key my-projectile-keymap-prefix (kbd "j r") 'projectile-regenerate-tags)
;; (define-key my-projectile-keymap-prefix (kbd "j j") 'projectile-find-tag)
;; ;; search: ack, ag
;; (define-key my-projectile-keymap-prefix (kbd "s a") 'projectile-ack)
;; (define-key my-projectile-keymap-prefix (kbd "s A") 'projectile-ag)
;; (define-key my-projectile-keymap-prefix (kbd "s g") 'projectile-grep)
;; (define-key my-projectile-keymap-prefix (kbd "s o") 'projectile-multi-occur)
;; (define-key my-projectile-keymap-prefix (kbd "s r") 'projectile-replace)
;; ;; buffer, file, dired, project ...
;; (define-key my-projectile-keymap-prefix (kbd "f b") 'projectile-switch-to-buffer)
;; (define-key my-projectile-keymap-prefix (kbd "f f") 'projectile-find-file)
;; (define-key my-projectile-keymap-prefix (kbd "f d") 'projectile-find-dir)
;; (define-key my-projectile-keymap-prefix (kbd "f l") 'projectile-find-file-in-directory)
;; (define-key my-projectile-keymap-prefix (kbd "f D") 'projectile-dired)
;; (define-key my-projectile-keymap-prefix (kbd "f S") 'projectile-save-project-buffers)
;; (define-key my-projectile-keymap-prefix (kbd "f k") 'projectile-kill-buffers)
;; (define-key my-projectile-keymap-prefix (kbd "f z") 'projectile-cache-current-file)
;; (define-key my-projectile-keymap-prefix (kbd "f F") 'projectile-find-file-in-known-projects)
;; (define-key my-projectile-keymap-prefix (kbd "f I") 'projectile-ibuffer)
;; (define-key my-projectile-keymap-prefix (kbd "f r") 'projectile-recentf)
;; (define-key my-projectile-keymap-prefix (kbd "f i") 'projectile-invalidate-cache)
;; (define-key my-projectile-keymap-prefix (kbd "s") 'projectile-switch-project)
;; ;; Test
;; (define-key my-projectile-keymap-prefix (kbd "t t") 'projectile-toggle-between-implementation-and-test)
;; (define-key my-projectile-keymap-prefix (kbd "t p") 'projectile-test-project)
;; ;; Shell
;; (define-key my-projectile-keymap-prefix (kbd "!") 'projectile-run-shell-command-in-root)
;; (define-key my-projectile-keymap-prefix (kbd "&") 'projectile-run-async-shell-command-in-root)
;; ;; Compile & Run
;; (define-key my-projectile-keymap-prefix (kbd "c") 'projectile-compile-project)
;; ;; Version Control (vc)
;; (define-key my-projectile-keymap-prefix (kbd "v") 'projectile-vc)


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
