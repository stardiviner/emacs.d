;;; init-erc.el --- init ERC for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Code:


(use-package erc
  :ensure t
  :defer t
  :config
  ;; [ user info ]
  (setq erc-nick "stardiviner"
        erc-try-new-nick-p nil
        erc-user-full-name "christopher M. miles"
        erc-email-userid "numbchild@gmail.com"
        )
  ;; [ password ]
  (setq erc-prompt-for-password nil
        erc-password (my/json-read-value my/account-file 'erc)
        )
  ;; encoding
  (setq  erc-server-coding-system '(utf-8 . utf-8))


  ;; [ NickServ ]
  (require 'erc-services)
  (add-to-list 'erc-modules 'services)
  (erc-update-modules)
  (erc-services-mode 1)
  ;; identify automatically
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-nickserv-passwords
        `((freenode ((,(eval erc-nick) . ,(my/json-read-value my/account-file 'erc))
                     ("Evanescence" . ,(my/json-read-value my/account-file 'erc))))
          ;; (oftc (("numbchild" . "PASSWORD")))
          )
        )

  ;; auto ghost yourself.
  (defun erc-ghost-maybe (server nick)
    "Send GHOST message to NickServ if NICK ends with `erc-nick-uniquifier'.
The function is suitable for `erc-after-connect'."
    (when (string-match (format "\\(.*?\\)%s+$" erc-nick-uniquifier) nick)
      (let ((nick-orig (match-string 1 nick))
            (password erc-session-password))
        (when (y-or-n-p (format "Current nick is '%s'. Do you want to ghost? " nick))
          (erc-message "PRIVMSG" (format "NickServ GHOST %s %s"
				                                 nick-orig password))
	        (erc-cmd-NICK nick-orig)
	        (erc-message "PRIVMSG" (format "NickServ identify %s %s"
				                                 nick-orig password))))))

  (add-hook 'erc-after-connect 'erc-ghost-maybe)

  ;; [ IRC Server ]
  (setq erc-server-auto-reconnect t)

  (setq erc-default-server "irc.freenode.net"
        erc-default-port 6667)

  ;; [ join & auto-join ]
  ;; make sure to use wildcards for e.g. freenode as the actual server
  ;; name can be be a bit different, which would screw up autoconnect
  (require 'erc-join)
  (erc-autojoin-mode t)
  (setq erc-autojoin-channels-alist
        '((".*\\.freenode.net" ; "freenode.net"
           "#emacs"
           ;; "#org-mode"
           ;; "#lisp"
           ;; "#clojure"
           ;; "#clojure-beginners"
           ;; "#archlinux"
           ;; "#swift-lang"
           ;; "#docker"
           ;; "#hackerrank"
           ;; "#dc5" ; #0xDEATHCODE
           )
          ))

  ;; [ disconnect ]
  ;;
  ;; If true, remove buffers associated with a server that is disconnected from
  ;; `erc-modified-channels-alist'.
  (setq erc-track-remove-disconnected-buffers nil)

  ;; [ SSL ]
  ;; SSL connections


  ;; start & switch
  (defun my-erc-start-or-switch ()
    "Connect to ERC, or switch to last active buffer."
    (interactive)
    (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
        (erc-track-switch-buffer 1) ;; yes: switch to last active
      ;; connections
      (erc
       :server "irc.freenode.net" :port 6667
       :nick "stardiviner" :password (my/json-read-value my/account-file 'erc)
       :full-name "christopher M. miles")
      ))

  (define-key irc-prefix (kbd "i") 'my-erc-start-or-switch)

  (define-key irc-prefix (kbd "j") 'erc-track-switch-buffer)
  (define-key irc-prefix (kbd "b") 'erc-iswitchb)

  ;; [ Tracking ]
  ;;
  ;; The next thing is tracking: ERC can track the various channels you're in, and
  ;; notify you when there is something new in the channel (it will colorize the
  ;; channelname in your modeline). This is generally useful, but I don't really
  ;; care about when people join or leave, or other IRC-meta spam. So:
  (require 'erc-track)
  ;; check channels
  (erc-track-mode t)
  (add-to-list 'erc-modules 'track)
  (erc-update-modules)
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "333" "353" "324" "329" "332" "477"))
  ;; ERC auto hide message types.
  (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE"
                        "333" "353" "324" "329" "332" "477")
        ;; erc-network-hide-list '(("freenode" "MODE")
        ;;                         ("OFTC" "JOIN" "QUIT"))
        ;; erc-channel-hide-list '(("#emacs" "QUIT" "JOIN")
        ;;                         ("#erc" "NICK"))
        )
  (setq erc-track-enable-keybindings t) ; [C-c C-SPACE], [C_c C-@]


  ;; [ netsplit ] -- Reduce JOIN/QUIT messages on netsplits.
  (require 'erc-netsplit)
  (erc-netsplit-mode t)


  ;; [ Matching ]
  (require 'erc-match)
  (erc-match-mode 1)
  ;; highlight keywords
  (setq erc-keywords '("\\<crack\\>" "\\<hack\\>"
                       ;; "\\<[R\|r]uby\\>" "\\<[L\|l]isp\\>" "\\<Go\\>" "\\<R\\>"
                       ))
  ;; [ Nicks Highlight ]
  (setq erc-current-nick-highlight-type 'nick-or-keyword)
  ;; [ Pals (friends) ]
  (setq erc-pals '("thuang"))
  ;; Fools (hide)
  (setq erc-fools nil) ; '()
  ;; If you want to highlight the fools using the appropriate face (dim gray or
  ;; similar), instead of removing their messages, then you must remove the
  ;; function ‘erc-hide-fools’ from ‘erc-text-matched-hook’.
  ;;
  ;; (remove-hook 'erc-text-matched-hook 'erc-hide-fools)


  ;; [ Complete ] -- [TAB]
  (require 'erc-pcomplete)
  (add-to-list 'erc-modules 'completion) ; aka `pcomplete'
  (erc-update-modules)
  (erc-pcomplete-mode 1)
  ;; (erc-completion-mode 1)

  (defun my:erc-completion-setup ()
    "Setup ERC completion."
    (make-local-variable 'company-minimum-prefix-length)
    (setq company-minimum-prefix-length 3))
  (add-hook 'erc-mode-hook #'my:erc-completion-setup)

  ;; (add-hook 'erc-mode-hook #'turn-on-auto-fill)
  (add-hook 'erc-mode-hook #'turn-on-visual-line-mode)

  (define-key erc-mode-map (kbd "C-c M-o") 'browse-url)

  ;; [ View ]

  (setq erc-header-line-uses-tabbar-p t)

  ;; ERC scroll to bottom
  ;; (require 'erc-goodies)
  ;; `erc-scrolltobottom-mode'
  ;; (add-to-list 'erc-modules 'scrolltobottom)
  ;; (erc-update-modules)

  ;; [ erc-scrolltoplace ] -- An ERC module to replace scrolltobottom while using keep-place.

  ;; this fix issues from `erc-scrolltobottom-mode'.

  (use-package erc-scrolltoplace
    :ensure t
    :config
    (add-to-list 'erc-modules 'scrolltoplace)
    (erc-update-modules)
    )

  ;; [ Filling & Wrap ]
  (require 'erc-fill)
  (setq erc-fill-column 78) ; default: 78
  (erc-fill-mode t)


  ;; [ timestamp ]
  (require 'erc-stamp)
  (add-to-list 'erc-modules 'stamp)
  (erc-update-modules)
  (erc-timestamp-mode t)
  ;; "[%H:%M]", "[%R-%m/%d]"
  (setq erc-timestamp-format "[%H:%M]")
  (setq erc-timestamp-use-align-to t
        erc-timestamp-right-column nil
        erc-timestamp-format-right "[%H:%M]"
        )

  (set-face-attribute 'erc-prompt-face nil
                      :inherit 'fixed-pitch)
  (set-face-attribute 'erc-timestamp-face nil
                      :inherit 'fixed-pitch)
  (set-face-attribute 'erc-nick-default-face nil
                      :inherit 'fixed-pitch)
  (set-face-attribute 'erc-nick-msg-face nil
                      :inherit 'fixed-pitch)
  (set-face-attribute 'erc-nick-prefix-face nil
                      :inherit 'fixed-pitch)

  (require 'erc-goodies)
  ;; interpret control characters
  (setq erc-interpret-controls-p t)
  ;; interpret mIRC colors
  (setq erc-interpret-mirc-color t)

  ;; Smiley
  (require 'erc-goodies)
  (add-to-list 'erc-modules 'smiley)
  (erc-update-modules)

  ;; [ TeX/LaTeX ] -- $ LaTeX $

  ;; Marking Emacs chat buffers as read (erc, jabber, etc).
  (defun my/mark-read ()
    "Mark buffer as read up to current line."
    (let ((inhibit-read-only t))
      (put-text-property
       (point-min) (line-beginning-position)
       'face       'font-lock-comment-face)))

  (defun my-bury-buffer ()
    "Bury buffer and maybe close its window."
    (interactive)
    (my/mark-read)
    (bury-buffer)
    (when (cdr (window-list nil 'nomini))
      (delete-window)))

  (with-eval-after-load 'erc
    (define-key erc-mode-map (kbd "<escape>") #'my-bury-buffer))
  (with-eval-after-load 'jabber
    (define-key jabber-chat-mode-map (kbd "<escape>") #'my-bury-buffer))


  ;; To remove the server part from the channel identifyer in the mode line,
  ;; customize ‘erc-mode-line-format’ and change "%s" to "%t".
  (setq erc-mode-line-format "%S %a")

  ;; prompt
  (setq erc-prompt " (λ) ⌨ ")
  (defun circadian:erc-faces (theme)
    "Reload customized faces on `circadian' `THEME' toggling."
    (set-face-attribute 'erc-prompt-face nil
                        :background (cl-case (alist-get 'background-mode (frame-parameters))
                                      ('light
                                       (color-darken-name (face-background 'default) 10))
                                      ('dark
                                       (color-darken-name (face-background 'default) 5)))
                        :foreground "lawn green"))
  (add-hook 'circadian-after-load-theme-hook #'circadian:erc-faces)


  ;; [ Button ]
  (require 'erc-button)
  (erc-button-mode 1)
  (setq erc-button-buttonize-nicks t)


  ;; [ Notify ]
  ;; Send notification on PRIVMSG or mentions.
  (require 'erc-desktop-notifications)
  (add-to-list 'erc-modules 'notifications)

  ;; notify on direct query message.
  ;; (defun ysph-erc-privmsg-notify (proc res)
  ;;   "Notify on direct query message in Emacs ERC."
  ;;   (cl-flet ((rtrim-string (s) (replace-regexp-in-string "\\([[:space:]\n]*$\\)" "" s)))
  ;;     (let ((channel-buffers     (erc-channel-list proc))
  ;;           (sender              (or (car (split-string (erc-response.sender res) "!"))
  ;;                                    (erc-response.sender res)))
  ;;           (target-channel-name (car (erc-response.command-args res)))
  ;;           (xwindow-class       (rtrim-string (shell-command-to-string "stumpish current-window-class"))))
  ;;       (unless (or (string= xwindow-class "Emacs") ; we are in an emacs frame
  ;;                   (member (get-buffer target-channel-name) channel-buffers)) ; this is a channel message
  ;;         (progn (notify "Instant message!"
  ;;                        (format "Direct message from %s" sender)
  ;;                        :icon     "/home/ysph/.emacs.d/emacs.png"
  ;;                        :timeout  120000
  ;;                        :app "ERC")
  ;;                nil        ; we never want this to interrupt processing
  ;;                )))))
  ;;
  ;; (add-hook 'erc-server-PRIVMSG-functions 'ysph-erc-privmsg-notify)


  ;; List of nicknames you want to be notified about online/offline status change.
  ;; This module defines a new command, /NOTIFY
  ;; (require 'erc-notify)
  ;; (add-to-list 'erc-modules 'notify)
  ;; (setq erc-notify-list '("tristan1" "yaxin"))

  ;; [ Sound ]
  (require 'erc-sound)
  (add-to-list 'erc-modules 'sound)
  (erc-sound-enable)
  (erc-update-modules)

  (setq erc-default-sound (concat user-emacs-directory "resources/audio/Hacking Game/hesfx_untold_tick2.wav"))

  (setq erc-sound-path `(,(concat user-emacs-directory "resources/audio/Hacking Game/")
                         ,(concat user-emacs-directory "resources/audio/Ingress/")))

  ;; Play sounds on some actions.
  (autoload 'org-clock-play-sound "org-clock")

  (defun my/erc-play-sound-started ()
    "Play sound when complete action in ERC."
    (org-clock-play-sound
     (concat user-emacs-directory
             "resources/audio/Hacking Game/track-intro.wav")
     ;; "voice-connecting.wav", "voice-welcome.wav"
     )
    )

  (defun my/erc-play-sound-hacking ()
    "Play sound when tick on ERC."
    (org-clock-play-sound
     (concat user-emacs-directory
             "resources/audio/Ingress/Speech/speech_hacking.wav")))

  (defun my/erc-play-sound-typing ()
    "Play sound when typing on ERC."
    (org-clock-play-sound
     (concat user-emacs-directory
             "resources/audio/Ingress/SFX/sfx_typing.wav")))

  (defun my/erc-play-sound-complete ()
    "Play sound when complete action in ERC."
    (org-clock-play-sound
     (concat user-emacs-directory
             "resources/audio/Hacking Game/voice-complete.wav")))

  (defun my/erc-play-sound-welcome ()
    "Play sound when entering ERC with welcome."
    (org-clock-play-sound
     (concat user-emacs-directory
             "resources/audio/Hacking Game/voice-welcome.wav")))

  (add-hook 'erc-join-hook #'my/erc-play-sound-welcome)

  (defun my/erc-play-sound-tick (str)
    "Play sound `str' when tick on ERC."
    (org-clock-play-sound
     (concat user-emacs-directory
             "resources/audio/Hacking Game/hesfx_untold_tick2.wav")))

  (add-hook 'erc-send-pre-hook #'my/erc-play-sound-tick)

  (defun my/erc-play-sound-tick2 ()
    "Play sound `str' when tick on ERC."
    (org-clock-play-sound
     (concat user-emacs-directory
             "resources/audio/Ingress/SFX/sfx_sonar.wav")))

  ;; TODO (add-hook 'erc-.. -hook #'my/erc-play-sound-tick2)

  ;; (add-hook 'erc-send-completed-hook #'my/erc-play-sound-tick)


  ;; Playing a sound when receiving a private message.
  (defun my/erc-play-sound-private (proc passed)
    "Play sound when receive a private message."
    (org-clock-play-sound
     (concat user-emacs-directory
             "resources/audio/Hacking Game/voice-incoming-transmission.wav")))

  (defun my/erc-play-sound-message (proc passed)
    "Play sound when receive a private message."
    (org-clock-play-sound
     (concat user-emacs-directory
             "resources/audio/Hacking Game/hesfx-newmessage.wav")))

  ;; For normal new essage in channel.
  ;; (add-hook 'erc-server-PRIVMSG-functions #'my/erc-play-sound-message t)
  ;; TODO: For private new message in channel.

  (defun my/erc-play-sound-confirm (proc nick login host to query)
    "Play sound when receive a private message."
    (org-clock-play-sound
     (concat user-emacs-directory
             "resources/audio/Hacking Game/voice-please-confirm.wav")))

  (add-hook 'erc-ctcp-query-DCC-hook #'my/erc-play-sound-confirm)

  ;; [ org-contacts + ERC ]


  ;; [ Input ]
  (setq erc-input-line-position -1)
  ;; use [RET] as send button for multiple lines input.
  (define-key erc-mode-map (kbd "<M-return>") 'newline)
  (define-key erc-mode-map (kbd "<return>") 'erc-send-current-line)
  (define-key erc-mode-map (kbd "<C-return>") 'erc-send-current-line)

  ;; [ Input History ]
  (require 'erc-ring)
  (add-to-list 'erc-modules 'ring)
  (erc-update-modules)
  (erc-ring-mode t)
  (setq erc-notice-highlight-type 'all)


  ;; [ Away ]
  (setq erc-away-nickname "stardiviner|away"
        erc-public-away-p nil)

  ;; auto away (low performance)
  ;; (add-to-list 'erc-modules 'autoaway)
  ;; (erc-update-modules)

  ;; manual away
  ;; channel away
  (defun my/erc-away (arg)
    "Mark away `ARG' from ERC."
    (interactive "sThe away reason: ")
    (erc-cmd-AWAY "arg"))
  (define-key erc-mode-map (kbd "C-c C-a") 'my/erc-away)
  ;; global all away
  (defun my/erc-away-global (arg)
    "Mark global away `ARG' in ERC."
    (interactive "sThe global away reason: ")
    (erc-cmd-GAWAY "arg"))
  (define-key erc-mode-map (kbd "C-c C-x") 'my/erc-away-global)


  ;; [ Logging ]
  ;; (require 'erc-log)
  ;; (add-to-list 'erc-modules 'log)
  ;; (require 'erc-truncate)
  ;; (add-to-list 'erc-modules 'truncate)
  ;; (erc-update-modules)
  ;; disable ERC logging
  (setq erc-enable-logging nil
        erc-log-channels-directory "~/.erc/logs/"
        ;; truncate buffers so they don't hog core.
        erc-truncate-buffer-on-save t
        erc-log-insert-log-on-open nil)


  ;; [ Spelling ]
  ;; this will slow down ERC typing in Emacs.
  (require 'erc-spelling)
  (erc-spelling-mode 1)
  ;; (setq erc-spelling-dictionaries '(("irc.tu-ilmenau.de" "german-new8")))
  (define-key erc-mode-map (kbd "<M-tab>") 'ispell-complete-word)

  ;; [ Speak ]
  ;; (require 'erc-speak)

  ;; [ Color ]
  ;; (use-package erc-colorize
  ;;   :ensure t
  ;;   :config
  ;;   (add-to-list 'erc-modules 'colorize)
  ;;   (erc-update-modules))

  (use-package erc-hl-nicks
    :ensure t
    :config
    (add-to-list 'erc-modules 'hl-nicks)
    (erc-update-modules))

  ;; [ Image ]
  ;; (use-package erc-image
  ;;   :ensure t
  ;;   :config
  ;;   ;; (erc-image-mode 1)
  ;;
  ;;   (setq erc-image-display-func 'erc-image-insert-inline
  ;;         erc-image-images-path "/tmp/"
  ;;         erc-image-inline-rescale 500 ; rescale the inline displayed image
  ;;         ;; erc-image-regex-alist
  ;;         )
  ;;   )

  ;; [ Bots ]

  ;; [ erc-babel ]
  ;; Usage:
  ;; - `erc-babel-toggle' :: toggle erc babel.
  ;; (setq erc-babel-include-orig t) ; include original message.
  ;; (define-key erc-mode-map (kbd "C-c C-S-t") 'erc-babel-toggle)
  ;; (define-key erc-mode-map (kbd "C-c C-S-o") 'erc-babel-include-orig-toggle)


  ;; [ Morse Code ]
  ;; (require 'erc-goodies)
  ;; (add-to-list 'erc-modules 'unmorse)
  ;; (erc-update-modules)


  ;; [ erc-crypt ] -- Symmetric Encryption for ERC.

  ;; (use-package erc-crypt
  ;;   :ensure t
  ;;   :config
  ;;   ;; (add-hook 'erc-mode-hook #'erc-crypt-enable) ; enable `erc-crypt' in ERC globally.
  ;;  
  ;;   ;; manually (toggle) keybinding for encrypting messages.
  ;;   (define-key erc-mode-map (kbd "C-c M-e") 'erc-crypt-mode)
  ;;   )

  ;; [ erc-social-graph ] -- A PieSpy-esque social network graph module for ERC.

  ;; (use-package erc-social-graph
  ;;   :ensure t
  ;;   :config
  ;;   (setq erc-social-graph-dynamic-graph t)
  ;;   (erc-social-graph-enable)
  ;;   )

  ;; Define new custom ERC commands.
  (defun erc-cmd-MYSYSTEM ()
    "Send my system version info."
    (let ((str (shell-command-to-string "uname -a")))
      (when str (erc-send-message str))))
  (defun erc-cmd-MYEMACS ()
    "Send my Emacs version info."
    (let ((str (emacs-version)))
      (when str (erc-send-message str))))
  (defun erc-cmd-MYORG-MODE ()
    "Send my Org-mode version info."
    (let ((str (org-version)))
      (when str (erc-send-message str))))

  ;; Search backwards, prompting to open any URL found. This is fantastic for ERC
  ;; buffers. I bind this to [C-c u] because I use it a lot.
  (defun browse-last-url-in-brower ()
    (interactive)
    (save-excursion
      (let ((ffap-url-regexp
             (concat
              "\\("
              "news\\(post\\)?:\\|mailto:\\|file:"
              "\\|"
              "\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://"
              "\\).")))
        (ffap-next t t))))

  (define-key erc-mode-map (kbd "C-c u") 'browse-last-url-in-brower)
  )


(provide 'init-erc)

;;; init-erc.el ends here
