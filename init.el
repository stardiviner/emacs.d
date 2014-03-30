



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
