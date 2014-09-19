;;; init-my-irc-erc.el --- init ERC for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:

;;; [ Help ]

;; - IRC: #erc on freenode.
;; - Mailing List: http://news.gmane.org/gmane.emacs.erc.general


;; [ Usage ]
;; - [TAB] :: completion for: nick,
;; - [M-TAB] :: ispell complete word
;; - [C-c C-j] :: join channel
;; - [C-c C-a] :: away channel
;; - [C-c C-x] :: away all channels
;; - [C-c C-p] :: part from channel
;; - [C-c C-q] :: quit server
;; - [M-n/p]   :: select next/previous command in history.
;; - [C-c C-u] :: kill input (undo)
;; - [C-c C-c] :: toggle interpret controls
;; - [C-c C-d] :: input action
;; - [Enter] :: on buttonized text to execute corresponding actions.
;; - [C-c C-w C-b/C-f] :: backward/forwards buttons.

;;; Code:

;;; [ ERC ]

(require 'erc)


;;; [ Modules ]

;; update modules
;; (erc-update-modules)

;; (setq erc-modules '(pcomplete netsplit fill button match track completion readonly networks ring autojoin noncommands irccontrols move-to-prompt stamp menu list erc-autoaway-mode))


;;; [ user info ]

(setq erc-nick "stardiviner"
      erc-user-full-name "christopher M. miles"
      erc-user-information "ERC User"
      erc-email-userid "numbchild@gmail.com"
      )


;;; [ password ]

(setq erc-prompt-for-password nil)

;; load password
(setq erc-password "chrisM.sprite324")

;; TODO: use GPG encrypted file to decrypt to get password.

;; TODO: read-lines function is not available
;; (let ((acc (read-lines "~/.emacs.d/my-init/.my-erc-account")))
;;   (setq erc-nick (car acc))
;;   (setq erc-password (nth 1 acc)))


;;; encoding

(setq  erc-server-coding-system '(utf-8 . utf-8))

;; (setq erc-encoding-coding-alist '(("#linuxfire" . chinese-iso-8bit)
;;                                   ("#foobar" . chinese-iso-8bit)
;;                                   ("#barfoo" . chinese-big5)))

;; (push '("#linuxfire" . (chinese-iso-8bit . chinese-iso-8bit))
;;       erc-encoding-coding-alist
;;       )
;; (push '("#jiye" . (chinese-iso-8bit . chinese-iso-8bit))
;;       erc-encoding-coding-alist
;;       )



;;; start & switch
(defun my-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer."
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    ;; (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
    ;;   )
    ;;
    ;; connections
    (erc
     :server "irc.freenode.net" :port 6667
     :nick "stardiviner" :password "chrisM.sprite324"
     :full-name "christopher M. miles")
    ;; (erc :server "irc.gimp.org" :port 6667 :nick "stardiviner" :full-name "christopher M. miles")
    ))

;; switch to ERC (c: chat)
(define-key my-tools-prefix-map (kbd "i") 'my-erc-start-or-switch)


;;; SSL connections

;;; TODO: after fix this, change freenode connect port to SSL port
;; (require 'tls)
;;
;; (setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof
;;                                        -CAfile /home/stardiviner/.ssl/certs/CAs.pem
;;                                        -cert /home/stardiviner/.ssl/certs/nick.pem"
;;                     "gnutls-cli --priority secure256
;;                                  --x509cafile /home/stardiviner/.ssl/certs/CAs.pem
;;                                  --x509certfile /home/stardiviner/.ssl/certs/nick.pem -p %p %h"
;;                     "gnutls-cli --priority secure256 -p %p %h"))


;;; [ NickServ ]

(require 'erc-services)
(erc-services-mode 1)

;; identify automatically
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords
      '((freenode (("stardiviner" . "chrisM.sprite324")
                   ("Evanescence" . "chrisM.sprite324")))
        ;; (oftc (("numbchild" . "chrisM.sprite324")))
        ))

;; (add-hook 'erc-after-connect
;;           '(lambda (SERVER NICK)
;;              (cond
;;               ((string-match "freenode\\.net" SERVER)
;;                ;; FIXME password here
;;                (erc-message "PRIVMSG" "NickServ identify chrisM.sprite324"))
;;               ;; ((string-match "oftc\\.net" SERVER)
;;               ;;  (erc-message "PRIVMSG" "NickServ identify chrisM.sprite324"))
;;               ;; ((string-match "jin\\.tekken" SERVER)
;;               ;;  (erc-message "PRIVMSG" "#bitlbee identify password3"))
;;               )))


;;; [ join & auto-join ]

;; Channels:
;; #emacs, #gnu, #gcc
;; #archlinux, #archlinux-cn, #kali-linux
;; #ubuntu-cn
;; #programming,
;; #lisp,

;; make sure to use wildcards for e.g. freenode as the actual server
;; name can be be a bit different, which would screw up autoconnect
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net"
         "#emacs"
         "#ruby"
         "#ubuntu-cn"
         ;; TODO: add startup command join #dc5.
         ;; "#dc5" ; BaseHTTPRequestHandler (in ~/.authinfo)
         )
        ))



;;; Disconnect

(add-hook 'erc-disconnected-hook '(erc-unset-network-name
                                   erc-modified-channels-update))

(setq erc-message-english-disconnected "\n\nConnection failed!  Re-establishing connection...\n"
      erc-track-remove-disconnected-buffers nil
      erc-message-english-disconnected-noreconnect "\n\nConnection failed!  Not re-establishing connection.\n"
      )

;;; Change header-line face on disconnect
;;; --- way 2
;; (defface erc-header-line-disconnected
;;   '((t (:foreground "black" :background "red")))
;;   "Face to use when ERC has been disconnected.")

;; (defun erc-update-header-line-show-disconnected ()
;;   "Use a different face in the header-line when disconnected."
;;   (erc-with-server-buffer
;;     (cond ((erc-server-process-alive) 'erc-header-line)
;;           (t 'erc-header-line-disconnected))))

;; (setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)


;;; [ Tracking ]
(require 'erc-track)

;; The next thing is tracking: ERC can track the various channels you're in, and
;; notify you when there is something new in the channel (it will colorize the
;; channelname in your modeline). This is generally useful, but I don't really
;; care about when people join or leave, or other IRC-meta spam. So:

;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                 "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;;; [ Matching ]
(require 'erc-match)

;; highlight keywords
(setq erc-keywords '("\\<crack\\>" "\\<hack\\>"
                     ;; "\\<[R\|r]uby\\>" "\\<[L\|l]isp\\>" "\\<Go\\>" "\\<R\\>"
                     ))


;;; Nicks Highlight
(setq erc-current-nick-highlight-type 'nick-or-keyword)

;; Pals
(setq erc-pals '("thuang"))
;; Fools (hide)
(setq erc-fools '())
;; If you want to highlight the fools using the appropriate face (dim gray or
;; similar), instead of removing their messages, then you must remove the
;; function ‘erc-hide-fools’ from ‘erc-text-matched-hook’.
;;
;; (remove-hook 'erc-text-matched-hook 'erc-hide-fools)


(erc-match-mode)

;;; [ Complete ]

;; - [TAB] ::

(add-hook 'erc-mode-hook
          '(lambda ()
             (require 'erc-pcomplete)
             (pcomplete-erc-setup)
             (erc-completion-mode 1)))


;;; disable line number mode
(add-hook 'erc-mode-hook
          '(lambda ()
             (linum-mode -1)))


;;; Filling & Wrap
(require 'erc-fill)
(erc-fill-mode t)

(setq erc-fill-column 78) ; default: 78
;;
;; If you’d like to have the fill width be a little more dynamic and change
;; properly when you resize a window, try this. (It might make things a little
;; ugly if you resize again later, though.) Note that only new lines will be
;; filled to the new value.
;;
;; (add-hook 'window-configuration-change-hook
;;           '(lambda ()
;;              (setq erc-fill-column (- (window-width) 2))))
;;
;; If you chat in multiple, differently sized ERC windows, it may be better to
;; make the erc-fill-column variable buffer-local:
(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook
          '(lambda ()
             (save-excursion
               (walk-windows
                (lambda (w)
                  (let ((buffer (window-buffer w)))
                    (set-buffer buffer)
                    (when (eq major-mode 'erc-mode)
                      (setq erc-fill-column (- (window-width w) 2)))))))))

(setq erc-fill-prefix "      + ")

;; Column around which all statically filled messages will be
;; centered.  This column denotes the point where the ' ' character
;; between <nickname> and the entered text will be put, thus aligning
;; nick names right and text left.
;;
;; (setq erc-fill-static-center 27)

;; If you have auto-fill-mode enabled by default for all new buffers, you might
;; need to disable it from the ‘erc-mode-hook’. Here’s how:
(add-hook 'erc-mode-hook (lambda () (auto-fill-mode 0)))


;;; timestamp
(erc-timestamp-mode t)
(setq erc-timestamp-format "[%H:%M]") ; "[%H:%M]", "[%R-%m/%d]"

;;; Control Characters
(setq erc-interpret-controls-p t)

;;; [ Colors ]
(setq erc-interpret-mirc-color t)


;; Remove Server Part from mode line
;;
;; To remove the server part from the channel identifyer in the mode line,
;; customize ‘erc-mode-line-format’ and change "%s" to "%t".

(setq erc-mode-line-format "%S %a %m")

;; prompt
(setq erc-prompt "暗月: ")

;;; [ Faces ]

;; http://www.emacswiki.org/emacs/ErcFaces

;; channel text, any other default text
(set-face-attribute 'erc-default-face nil
                    :foreground "gray")
;; private messages, notices
(set-face-attribute 'erc-direct-msg-face nil
                    :foreground "indian red")
;; the commands and text you typed
(set-face-attribute 'erc-input-face nil
                    :foreground "gray"
                    :weight 'bold
                    )
(set-face-attribute 'erc-bold-face nil
                    :weight 'bold
                    )
(set-face-attribute 'erc-inverse-face nil
                    :inverse-video t
                    )
(set-face-attribute 'erc-underline-face nil
                    :underline t
                    )
;; the input prompt text
(set-face-attribute 'erc-prompt-face nil
                    :background "black" :foreground "#444444"
                    :box '(:color "dark red" :line-width 1 :style nil)
                    )
;; server notices (join and leave messages; for notices from other users use ‘erc-direct-msg-face’)
(set-face-attribute 'erc-notice-face nil
                    :foreground "white"
                    )
;; actions in channels, queries and ctcp action
(set-face-attribute 'erc-action-face nil
                    :foreground "orange red"
                    )
;; server and ERC error messages
(set-face-attribute 'erc-error-face nil
                    :foreground "red"
                    :weight 'bold
                    )
;; the timestamp
(set-face-attribute 'erc-timestamp-face nil
                    :foreground "#444444"
                    )
;; nicknames in channels
(set-face-attribute 'erc-nick-default-face nil
                    )
;; nicknames in messages and private notices
(set-face-attribute 'erc-nick-msg-face nil
                    :foreground "green" :background "black"
                    )

;;; The following faces are also available with erc-match is loaded (see ErcHighlighting for more details)
;; Pals
(set-face-attribute 'erc-pal-face nil
                    :foreground "green"
                    )
;; Fools
(set-face-attribute 'erc-fool-face nil
                    :foreground "dim gray"
                    )
;; just the nickname or nickname and text of text sent from those in your ‘erc-dangerous-hosts’
(set-face-attribute 'erc-dangerous-host-face nil
                    :foreground "red"
                    :weight 'bold
                    )
;; matches to keywords you have set with ‘erc-keywords’
(set-face-attribute 'erc-keyword-face nil
                    :foreground "cyan" :background "#444444"
                    :box '(:color "cyan" :line-width 1 :style nil)
                    :weight 'bold
                    )
;; occurrences of your current nickname
(set-face-attribute 'erc-current-nick-face nil
                    :foreground "cyan" :background "black"
                    :box '(:color "cyan" :line-width 1 :style nil)
                    )


;;; [ Buttons ]

;; buttonizes all messages according to erc-button-alist (*slow*)
(erc-button-mode t)

;; ErcButton gives you the ability to “buttonize” text. Once a piece of text is
;; buttonized, you can [middle-click] it or hit [Enter] on it to trigger the
;; corresponding action. You can jump to the last button with [C-c C-w C-b], and
;; to the next button with [C-c C-w C-f].

;; "append" more to variable
;; (setq erc-button-alist)

(setq erc-button-buttonize-nicks t)


;;; ERC all lines are read-only.
;; you can toggle this in erc-modules.


;;; Erc Smileys

;; (require 'smiley)

;; (add-to-list 'smiley-regexp-alist '("\\(:-?]\\)\\W" 1 "forced"))
;; (add-to-list 'smiley-regexp-alist '("\\s-\\(:-?/\\)\\W" 1 "wry"))
;; (add-to-list 'smiley-regexp-alist '("\\(:-?(\\)\\W" 1 "sad"))
;; (add-to-list 'smiley-regexp-alist '("\\((-?:\\)\\W" 1 "reverse-smile"))
;; (add-to-list 'smiley-regexp-alist '("\\(:-?D\\)\\W" 1 "grin"))
;; (add-to-list 'smiley-regexp-alist '("\\(:-?P\\)\\W" 1 "poke"))


;;; [ Notify ]

;;; Ring
(require 'erc-ring)
(erc-ring-mode t)

(setq erc-notice-highlight-type 'all)


;;; Sound

;; TODO http://www.emacswiki.org/emacs/ErcSound


;;; netsplit -- hide join, quit messages.
(require 'erc-netsplit)
(erc-netsplit-mode t)


;;; BBDB

;; TODO http://www.emacswiki.org/emacs/ErcBbdb


;;; [ Input ]

(setq erc-input-line-position -2)

;;; [ Spelling ]
;; NOTE: this will slow down ERC typing in Emacs.
(erc-spelling-mode -1)

;; (setq erc-spelling-dictionaries '(("irc.tu-ilmenau.de" "german-new8")))


;;; [ Speak ]

;; (require 'erc-speak)


;;; [ Image ]

;; (unless (package-installed-p 'erc-image)
;;   (package-install 'erc-image))
;; (require 'erc-image)
;;
;; (erc-image-mode 1)

;; (setq
;;  ;; erc-image-regex-alist
;;  erc-image-display-func 'erc-image-issert-inline
;;  erc-image-images-path "/tmp/"
;;  erc-image-inline-rescale 50 ; rescale the inline displayed image
;;  )

;;; [ Bots ]

;; http://www.emacswiki.org/emacs/ErcRobot
;; http://www.emacswiki.org/emacs/BirnyTheBot
;; http://www.emacswiki.org/emacs/ErBot


;;; [ Part & Quit ]
(setq erc-part-reason 'erc-part-reason-various)
(setq erc-quit-reason 'erc-quit-reason-various)

(setq erc-part-reason-various-alist
      '(("aha" erc-part-reason-zippy)
        ("xmms" dme:now-playing)
        ("version" erc-part-reason-normal)
        ("home" "Gone home !")
        ("^$" "No SEX no world!")))

;; auto close buffer/window when part from channel.
(setq erc-kill-buffer-on-part t)

;;; Quit
(setq erc-kill-server-buffer-on-quit t
      erc-kill-queries-on-quit t)

;;; Away
(setq erc-auto-set-away t
      erc-away-nickname "stardiviner|away"
      ;; erc-autoaway-idle-seconds 1200
      )

;; channel away
(define-key erc-mode-map (kbd "C-c C-a")
  '(lambda (arg)
     (interactive "sThe away reason: ")
     ;; TODO:
     ;; (if (arg == "")
     ;;     (let arg "No sex, no code!"))
     (erc-cmd-AWAY "arg")
     ))
;; global all away
(define-key erc-mode-map (kbd "C-c C-x")
  '(lambda (arg)
     (interactive "sThe global away reason: ")
     (erc-cmd-GAWAY "arg")
     ))

;;; custom join key binding [C-c C-j] for some channels need "key"
;; (erc-join-channel CHANNEL & optional key)



;;; [ Auto Query ]

;; Here’s how to have query buffers open automatically when someone sends a private message.

(setq erc-auto-query 'window-noselect)

;; As a default, only private messages trigger automatic creation of query
;; buffers. If you’d like have the same behavior when you receive notices you
;; can use the following:
;;
;; (add-hook 'erc-after-connect
;;           (lambda (server nick)
;;             (add-hook 'erc-server-NOTICE-hook 'erc-auto-query)))


;;; [ query ]

;; > /query [NICK]

(setq erc-query-display 'window)


;;; [ Logging ]

(setq erc-log-channels-directory "~/.erc/logs/")

;;; Truncate buffers so they don't hog core.
;;
(setq erc-max-buffer-size 20000)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(setq erc-truncate-buffer-on-save t)


;;; erc-babel

;;; http://www.emacswiki.org/emacs/erc-babel.el

;;; Usage:
;;; - `erc-babel-toggle' :: toggle erc babel.
;;; -

;; (setq erc-babel-include-orig t) ; include original message.

;; (define-key erc-mode-map (kbd "C-c C-S-t") 'erc-babel-toggle)
;; (define-key erc-mode-map (kbd "C-c C-S-o") 'erc-babel-include-orig-toggle)


;;; switch erc buffers

;;; default [C-c C-b] erc-iswitchb.

;;; Using ido select with ERC
(defun stardiviner/erc-buffer-switch-with-ido ()
  (interactive)
  (switch-to-buffer
   (ido-completing-read "Channel:"
                        (save-excursion
                          (delq
                           nil
                           (mapcar (lambda (buf)
                                     (when (buffer-live-p buf)
                                       (with-current-buffer buf
                                         (and (eq major-mode 'erc-mode)
                                            (buffer-name buf)))))
                                   (buffer-list)))))))

;;; select with helm
(defun stardiviner/erc-buffer-switch-with-helm ()
    ""
  (interactive)
  (switch-to-buffer
   (helm-comp-read "Channel:"
                   (save-excursion
                     (delq
                      nil
                      (mapcar (lambda (buf)
                                (when (buffer-live-p buf)
                                  (with-current-buffer buf
                                    (and (eq major-mode 'erc-mode)
                                         (buffer-name buf)))))
                              (buffer-list)))))))

(define-key erc-mode-map (kbd "C-c b") 'stardiviner/erc-buffer-switch-with-helm)


;;; Morse Code

;; http://www.emacswiki.org/emacs/ErcMorseCode



(provide 'init-my-irc-erc)

;;; init-my-irc-erc.el ends here
