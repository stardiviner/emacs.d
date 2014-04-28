;;; init-ido.el --- init ido settings.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

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




(provide 'init-ido)

;;; init-ido.el ends here
