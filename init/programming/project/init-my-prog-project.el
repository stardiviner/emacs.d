;;; init-my-prog-project.el --- init Project settings for Emacs

;;; Commentary:


;;; Code:


(unless (boundp 'my-prog-project-map)
  (define-prefix-command 'my-prog-project-map))
(global-set-key (kbd "C-c p") 'my-prog-project-map)


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


(use-package projectile
  :ensure t
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  
  :config
  ;; Caching: nil, (* 10 60) [10 minutes],
  (setq projectile-enable-caching t
        ;; remote file exists cache expire to 10 minutes
        projectile-file-exists-remote-cache-expire '(* 30 60)
        )
  ;; using Projectile everywhere
  (setq projectile-require-project-root t)
  ;; Completion Options
  (setq projectile-completion-system 'ivy
        projectile-use-git-grep t ; use `vc-git-grep'
        )

  ;; test
  (setq projectile-create-missing-test-files t)

  ;; custom default projectile mode-line :lighter for my custom mode-line format.
  (setq projectile-mode-line '(:eval
                               (format "(%s)" ; " Projectile/>[%s]"
                                       (projectile-project-name))))

  ;; ignored projects.
  ;; (add-to-list projectile-globally-ignored-modes)
  ;; (add-to-list projectile-globally-ignored-directories)
  ;; (add-to-list projectile-globally-ignored-file-suffixes)
  ;; (add-to-list projectile-globally-ignored-files)


  ;; Toggle `projectile-mode'
  (projectile-global-mode +1)
  ;; OR
  ;; (dolist (hook
  ;;          '(prog-mode-hook
  ;;            ))
  ;;   (add-hook hook 'projectile-on))

  ;; [ redefine projectile keybindings ]
  ;;
  ;; (unless (boundp 'my-projectile-keymap-prefix)
  ;;   (define-prefix-command 'my-projectile-keymap-prefix))
  ;; (global-set-key (kbd "C-c p") 'my-projectile-keymap-prefix)
  )



(provide 'init-my-prog-project)

;;; init-my-prog-project.el ends here
