;;; init-my-emacs-completion.el --- my Emacs completion frameworks init

;;; Commentary:


;;; Code:

;;; [ complete ]

;; (setq tab-always-indent 'complete)
;; (add-to-list 'completion-styles 'initials t)
;; (setq completion-cycle-threshold 5)
(setq completion-at-point-functions 'auto-complete)


;;; [ pcomplete ] --- Programmable, Context-Sensitive Completion Library

(load-library "pcomplete")


;;; [ Icomplete ]

;; (icomplete-mode 1)



;;; Press [TAB] in minibuffer to show completions in popup window buffer.


;;; [ ido ]

(require 'ido)
(require 'ido-ubiquitous)
(ido-mode t)                            ; enable ido mode
(ido-everywhere t)                      ; use ido-mode wherever possible
(ido-ubiquitous-mode t)                 ; enable ido-ubiquitous

(setq ido-enable-prefix nil
      ido-enable-flex-matching 't       ; enable fuzzy search
      ido-create-new-buffer 'prompt
      ido-use-filename-at-point 'guess  ; look for filename at point
      ido-max-prospects 10
      ido-use-virtual-buffers 't        ; allow me to open closed buffers, even
      ido-auto-merge-work-directories-length -1
      ido-default-buffer-method 'selected-window ; allow buffer to be open in different frames
      ido-save-directory-list-file (expand-file-name "ido.hist" user-emacs-directory)
      )


;;; [ ido-vertical-mode ] -- vertical ido.

;;; Usage:
;; - [M-x]
;; - [C-n/p]

(require 'ido-vertical-mode)

(ido-mode 1)
(ido-vertical-mode 1)

;; ---------------------------------------------------------------------------------
;; (setq ido-decorations '("{" "}" " | " " | ..." "[" "]"
;;                         " [No match]" " [Matched]" " [Not readable]"
;;                         " [Too big]" " [Confirm]")
;;       ido-vertical-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]"
;;                                  " [No match]" " [Matched]" " [Not readable]"
;;                                  " [Too big]" " [Confirm]"
;;                                  "\n-> " "")
;;       )
;; ---------------------------------------------------------------------------------


;;; [ flx-ido ]

;; (require 'flx-ido)


;;; [ smex ] --- remember recently and most frequently used commands.

;; work with ido will show recent frequently used commands in [M-x].

(require 'smex)

(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;; (require 'init-helm)
(require 'init-auto-complete)
;; (require 'init-company-mode)



(provide 'init-my-emacs-completion)

;;; init-my-emacs-completion.el ends here
