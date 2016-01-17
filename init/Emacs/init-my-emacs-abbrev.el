;;; init-my-emacs-abbrev.el --- abbrev mode init

;;; Commentary:


;;; Code:

;;; [ abbrev-mode ]

(require 'abbrev)

(setq abbrev-file-name "~/.emacs.d/init/abbrevs/abbrev_defs")
(setq save-abbrevs t)     ; save abbrevs when files are saved, nil: stop asking.
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file) ; reads the abbreviations file on startup quietly.
  )

(define-abbrev-table 'global-abbrev-table
  '(("mynick" "stardiviner")
    ("myemail" "numbchild@gmail.com")
    ("mygmail" "numbchild@gmail.com")
    ("mygmailformat" "numbchild[@]{gmail}.com")
    ("myhomepage" "http://stardiviner.github.io")
    ("mytwitter" "@numbchild")
    ("myqq" "348284894")
    ))

(setq-default abbrev-mode t)            ; turn on abbrev mode globally.


;;; [ dabbrev ]
;; Usage:
;; - [M-/] -- dabbrev-expand
(require 'dabbrev)




(provide 'init-my-emacs-abbrev)

;;; init-my-emacs-abbrev.el ends here
