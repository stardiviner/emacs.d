;;; init-my-emacs-abbrev.el --- abbrev mode init

;;; Commentary:


;;; Code:

;;; [ abbrev-mode ]

(require 'abbrev)

;;; setup my abbrevs file custom path.
(setq abbrev-file-name "~/.emacs.d/init/abbrevs/abbrev_defs")
(setq save-abbrevs 'silently) ; save abbrevs when files are saved, nil: stop asking.
(setq-default abbrev-mode t)
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

;;; define more keybindings
(define-key abbrev-map (kbd "/") 'exapnd-abbrev)
(define-key abbrev-map (kbd "e") 'edit-abbrevs)
(define-key abbrev-map (kbd "M-l") 'list-abbrevs)
(define-key abbrev-map (kbd "i") 'inverse-add-abbrev)

(setq-default abbrev-mode t)            ; turn on abbrev mode globally.


;;; [ dabbrev ] -- Dynamic Abbrevs [M-/]

(require 'dabbrev)


(provide 'init-my-emacs-abbrev)

;;; init-my-emacs-abbrev.el ends here
