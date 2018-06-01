;;; init-rcirc.el --- init for rcirc.

;;; Time-stamp: <2018-06-01 11:58:10 stardiviner>

;;; Commentary:

;;; Usage:
;;;
;;; [M-x irc]

;;; Code:

(require 'rcirc)

;;; server, account info
(setq rcirc-default-nick "stardiviner"
      rcirc-default-user-name "stardiviner"
      rcirc-default-full-name "Christopher M. Miles")

(setq rcirc-authinfo
      `(("freenode" nickserv "stardiviner" ,(my/json-read-value my/account-file 'erc))
        ;; ("freenode" chanserv "stardiviner" )
        ;; ("bitlbee" bitlbee "stardiviner" "PASSWORD")
        ))

(setq rcirc-server-alist
      '(("irc.freenode.net"
         :channels ("#emacs" "#lisp" "#clojure" "#archlinux")
         :nick "stardiviner"
         :user-name "stardiviner"
         :port 6667)))
;; (add-to-list 'rcirc-server-alist
;;              '("irc.gimp.org" :channels ("#gimp")))
;; (add-to-list 'rcirc-server-alist
;;              '("irc.gnome.org" :channels ("#gnome")))
;; (add-to-list 'rcirc-server-alist
;;              '("irc.oftc.net" :channels ()))

;;; Reconnecting after you have lost the connection with command `/reconnect'.

;;; [ Channel ] -- [C-c C-l] (set low priority for very active channel)
;;; [ track mode ] -- [C-c C-<SPC>]
(add-hook 'rcirc-mode-hook #'rcirc-track-minor-mode)

;;; [ People ] -- `/ignore', `/bright', `/dim'

;;; [ Highlighting ] -- `/keyword'

;;; [ Notices ] -- [C-c C-o] omit notices.

(add-to-list 'rcirc-omit-responses "AWAY")

;;; [ Edit ]

(add-hook 'rcirc-mode-hook #'flyspell-mode)



(provide 'init-rcirc)

;;; init-rcirc.el ends here
