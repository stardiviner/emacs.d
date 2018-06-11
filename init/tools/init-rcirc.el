;;; init-rcirc.el --- init for rcirc.

;;; Time-stamp: <2018-06-04 14:04:25 stardiviner>

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

;;; /msg NickServ identify <password>
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
         :encryption tls :port 6697)))
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

;;; ("JOIN" "PART" "QUIT" "NICK")
(add-to-list 'rcirc-omit-responses "AWAY")
(add-hook 'rcirc-mode-hook #'rcirc-omit-mode)

;;; [ Edit ]

(add-hook 'rcirc-mode-hook #'flyspell-mode)

(define-key tools-prefix (kbd "i") 'rcirc)



(provide 'init-rcirc)

;;; init-rcirc.el ends here
