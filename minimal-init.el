;;; init.el --- Emacs init file.

;;; Commentary:


;;; Code:

;;; [ package manager ]

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
        ;;        :type github
        ;;        :pkgname "kbkbkbkb1/guide-key"
	;;        :features guide-key)
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
         auto-complete
         auto-complete-yasnippet auto-complete-chunk
         auto-complete-etags auto-complete-pcmp
         auto-complete-emacs-lisp
         auto-complete-ruby auto-complete-clang auto-complete-c-headers
         auto-complete-css auto-complete-nxml
         auto-complete-verilog
         auto-complete-latex
         ;; company-mode
         )
       (mapcar 'el-get-source-name el-get-extra-sources)))

(el-get 'sync my-el-get-packages)


;;; [ package.el ]

;; (require 'package)
;; (setq package-archives
;;       '(("marmalade" . "http://marmalade-repo.org/packages/")
;;         ("org"       . "http://orgmode.org/elpa/")
;;         ("melpa"     . "http://melpa.milkbox.net/packages/")))
;; (package-initialize)


;;; add my init files directory
(let ((default-directory "~/.emacs.d/init/"))
  (setq load-path
        (append
          (let ((load-path (copy-sequence load-path))) ; shadow
            (append
              (copy-sequence (normal-top-level-add-to-load-path '(".")))
              (normal-top-level-add-subdirs-to-load-path)))
          load-path)))


;;; auto-complete

(require 'auto-complete)
(require 'auto-complete-config)
(require 'popup)

(global-auto-complete-mode 1)

(setq ac-auto-start 2)
(setq ac-delay 0.2) ; delay time to start completion in real number seconds
;; (setq ac-show-menu-immediately-on-auto-complete t) ; it is a trade off of responsibility and performance
(setq ac-auto-show-menu 0.3) ;; show popup menu after how many seconds
(setq ac-menu-height 10) ; smaller ac-menu is more cute. big ac-menu is not necessary.
(setq-default ac-expand-on-auto-complete t) ; Non-nil means expand whole common part on first time `auto-complete'.
(setq-default ac-dwim nil) ; to get pop-ups with docs even if a word is uniquely completed.

;; (ac-set-trigger-key "TAB") ; usualy this, <tab> has higher priority than TAB.
;; (ac-set-trigger-key "<tab>")
;; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(define-key global-map (kbd "M-TAB") 'ac-fuzzy-complete) ; fuzzy complete.
;;; ac-menu-map keymap only map for menu is available, not break default.
(setq ac-use-menu-map t)       ; nil: to disable default ac-menu-map key bindings.
;; disable [<tab>] [C-n/p] -> ac-next in ac-menu.
(define-key ac-menu-map (kbd "<tab>") nil)
(define-key ac-menu-map (kbd "<S-tab>") nil) ; "S-TAB". "?\\s-\\t"
(define-key ac-menu-map (kbd "C-n") nil)
(define-key ac-menu-map (kbd "C-p") nil)
(define-key ac-menu-map (kbd "M-SPC") 'ac-complete) ; select current candidate.
(define-key ac-menu-map (kbd "M-j") 'ac-complete) ; select current candidate.
(define-key ac-menu-map (kbd "M-n") 'ac-next) ; next candidate.
(define-key ac-menu-map (kbd "M-p") 'ac-previous) ; previous candidate.
(define-key ac-menu-map (kbd "M-i") 'ac-expand) ; for expand snippet, abbrev etc.
(define-key ac-menu-map (kbd "C-s") 'ac-isearch)
(define-key ac-menu-map (kbd "M-s") 'ac-isearch) ; isearch in popup menu.
(define-key ac-menu-map (kbd "C-i") 'ac-expand-common) ; complete common string.
(define-key ac-menu-map (kbd "C-h") 'ac-stop) ; close the auto complete popup menu.
(define-key ac-menu-map (kbd "<return>") 'newline) ; go to new line.
;; TODO:
;; (define-key ac-menu-map (kbd "<tab>") ; dwim to expand the candidate: yasnippet, abbrev, template, etc.
;;   (defun my-expand-candidate-dwim ()
;;     (if (yas/expand-snippet 'candidate)
;;         (yas/expand))))
(setq ac-use-quick-help t) ; nil to disable auto popup quick help.
(setq ac-quick-help-delay 1.3)
(setq ac-quick-help-height 20)
;; buffer help
(define-key ac-completing-map (kbd "M-h") 'ac-quick-help)
(define-key ac-mode-map (kbd "C-c h") 'ac-last-quick-help)
(define-key ac-mode-map (kbd "C-c H") 'ac-last-help)
;; TODO: (define-key popup-keymap (kbd "C-M-v") 'popup-scroll-down)

(require 'pos-tip)

;; (setq ac-use-fuzzy t)

(setq ac-ignore-case 'smart)

(setq ac-disable-faces '(font-lock-comment-face font-lock-string-face font-lock-doc-face)
      ac-disable-inline nil               ; disable inline completion visibility
      )

(setq ac-comphist t)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-dict")

(setq-default ac-sources
              '(;; snippet
                ac-source-yasnippet
                ac-source-filename
                ac-source-files-in-current-dir
                ;; ac-source-semantic
                ;; ac-source-etags
                ac-source-abbrev
                ;; ac-source-dabbrev
                ac-source-words-in-buffer
                ac-source-words-in-same-mode-buffers
                ac-source-dictionary
                ))

(setq ac-modes
      (append ac-modes
              '(prog-mode               ; programming modes
                text-mode org-mode markdown-mode
                change-log-mode
                ;; objc-mode
                ;; sql-mode js3-mode
                makefile-mode makefile-gmake-mode makefile-automake-mode
                autoconf-mode
                snippet-mode)))

(ac-flyspell-workaround)
(ac-linum-workaround)


;;; [ auto-complete-emacs-lisp ]

(require 'auto-complete-emacs-lisp)

;; this add emacs lisp source into AC, and support show popup help doc.
(dolist (hook '(emacs-lisp-mode-hook
                eval-expression-minibuffer-setup-hook
                ))
  (add-hook hook 'ac-emacs-lisp-mode-setup))
