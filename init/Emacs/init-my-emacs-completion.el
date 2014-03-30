;;; init-my-emacs-completion.el --- my Emacs completion frameworks init

;;; Commentary:


;;; Code:

;;; [ pcomplete ] --- programmable completion
(load-library "pcomplete")


;;; [ Icomplete ]

;; (icomplete-mode 1)


;;; Press [TAB] in minibuffer to show completions in popup window buffer.


;;; [ ido ]



;;; [ ido-vertical-mode ] -- vertical ido.

;;; Usage:
;; - [M-x]
;; - [C-n/p]

(require 'ido-vertical-mode)

(ido-mode 1)
(ido-vertical-mode 1)

;; (setq ido-decorations '("{" "}" " | " " | ..." "[" "]"
;;                         " [No match]" " [Matched]" " [Not readable]"
;;                         " [Too big]" " [Confirm]")
;;       ido-vertical-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]"
;;                                  " [No match]" " [Matched]" " [Not readable]"
;;                                  " [Too big]" " [Confirm]"
;;                                  "\n-> " "")
;;       )



;;; [ Helm ] --- (incremental completion and selection narrowing framework)
;; Customize:
;; - [C-c c] -- for all complete framework prefix.

;; Basic usage:
;;  - M-x helm-mini
;; general helm commands:
;;  - [TAB] -- access to `action' menu with
;;  - [C-z] -- use persistent actions with
;;  - [M-SPACE] -- mark candidate with
;; get helm help in heml minor mode:
;;  - [C-h m]
;; wildcard match
;;  - ~/_esk (here `_' is a space)

;; (unless (package-installed-p 'helm)
;;   (package-install 'helm))

;; (require 'helm)
;; (require 'helm-config)

;; (helm-mode 1)

;; (set-face-attribute 'helm-selection nil
;;                     :background "#004A5D" :foreground "white"
;;                     :box '(:color "cyan" :line-width 1)
;;                     :weight 'bold)

;; (global-set-key (kbd "C-c h") 'helm-mini)

;; (setq helm-case-fold-search t) ; whether toggle case sensitive search depend on your input has mixture of upcase and downcase.

;;; Bookmark
;; Helm bookmarks[C-x C-x r b]
;; (helm-highlight-bookmark)

;; Firefox bookmarks [C-x C-x]
;; NOTE config your firefox `about:config' to enable:
;; user_pref("browser.bookmarks.autoExportHTML", false);





(provide 'init-my-emacs-completion)

;;; init-my-emacs-completion.el ends here
