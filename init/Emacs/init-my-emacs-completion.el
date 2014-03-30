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







(provide 'init-my-emacs-completion)

;;; init-my-emacs-completion.el ends here
