



;;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq el-get-verbose t)

;;; extra sources which not in el-get recipes.
(setq el-get-extra-sources
      '(
	;; (:name guide-key
        ;;        :description "Guide following keys to an input key sequence automatically and dynamically in Emacs."
	;;        :depends popwin
        ;;        :type git
        ;;        :url "https://github.com/kbkbkbkb1/guide-key")
	))

;;; my packages which will be installed.
(setq my-el-get-packages
      (append
       '(popup
         pos-tip
         popup-pos-tip
         showtip
         tooltip-help
         ;; Emacs
         ;; completion
         ido-vertical-mode
         helm
         helm-descbinds
         auto-complete
         company-mode
         ;; color theme
         color-theme-solarized
         ;; buffer & window, frame
         window-number ; switch-window
         workgroups2 ; e2wm
         popwin
         ;; edit
         undo-tree
         multiple-cursors
         ;; bookmark, register, macro,
         bm
         ;; keybinding
         guide-key
         ;; dired
         direx
         ;; search
         isearch+ visual-regexp visual-regexp-steroids ace-jump-mode
         ack-and-a-half
         ;; Org-mode
         org-fstree org-bullets
         ;; tools
         calfw sauron appt
         gist
         )
       (mapcar 'el-get-source-name el-get-extra-sources)))

(el-get 'sync my-el-get-packages)



;;; add my init files directory
(let ((default-directory "~/.emacs.d/init/"))
  (setq load-path
        (append
          (let ((load-path (copy-sequence load-path))) ; shadow
            (append
              (copy-sequence (normal-top-level-add-to-load-path '(".")))
              (normal-top-level-add-subdirs-to-load-path)))
          load-path)))



;;; my custom functions
(require 'init-my-functions)

;;; Emacs
(require 'init-my-emacs-completion)
(require 'init-my-emacs-environment)
(require 'init-my-emacs-settings)
(require 'init-my-emacs-help)
(require 'init-my-emacs-font)
(require 'init-my-emacs-popup)
(require 'init-my-emacs-highlight)
(require 'init-my-emacs-minibuffer)
(require 'init-my-emacs-buffer)
(require 'init-my-emacs-window)
(require 'init-my-emacs-edit)
(require 'init-my-emacs-bookmark)
(require 'init-my-emacs-key-bindings)
(require 'init-my-emacs-indent)
(require 'init-my-emacs-electric)
(require 'init-my-emacs-spell)
(require 'init-my-emacs-file)
(require 'init-my-emacs-image)
(require 'init-my-emacs-dired)
(require 'init-my-emacs-tramp)
(require 'init-my-emacs-modes)
(require 'init-my-emacs-abbrev)
(require 'init-my-emacs-search)
(require 'init-my-emacs-regexp)
(require 'init-my-emacs-calendar)
(require 'init-my-emacs-vcs)
(require 'init-my-emacs-shell)
(require 'init-my-emacs-calculator)
(require 'init-my-emacs-encrypt)
(require 'init-my-emacs-customize)



;;; Tools
(require 'init-my-tool-org-mode)
(require 'init-my-tool-dict)
;; (require 'init-my-tool-bbdb)
(require 'init-my-tool-sauron)
(require 'init-my-tool-paste)
(require 'init-my-tool-w3m)
(require 'init-my-tool-diagram)
;; (require 'init-my-tool-emms)
;; (require 'init-my-tool-speak)
;; (require 'init-my-tool-newsticker)


;;; Email
(require 'init-my-email-message-mode)
(require 'init-my-email-mu4e)
