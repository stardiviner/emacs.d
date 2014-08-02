;;; init-my-tool-sauron.el --- init sauron

;;; Commentary:

;;; https://github.com/djcb/sauron

;;; what is it?

;; sauron is an emacs mode for keeping track of events happening in the (emacs)
;; world around you. Events are things like ‘appointment in 5 minutes’, ‘bob
;; pinged you on IRC’, ‘torrent download is complete’ etc. Sauron shows those
;; events like a list – basically like a log. You can ‘activate’ an event by
;; either pressing RET when point is on it, or clicking it with the middle mouse
;; button (<mouse-2>).

;; When activated, it can execute some arbitrary function – for example in the
;; case of IRC (ERC), it will switch you to the buffer (channel) it originated
;; from. It’s a bit of a generalization of what tracking mode does in ERC (the
;; emacs IRC client), and that is in fact how it started.

;; There’s an increasing number of hooks and tunables in sauron, which allows
;; you to fine-tune the behavior. However, I strive for it to be useful with
;; minimal configuration.


;;; Usage:

;; - [C-c n] :: sauron notify
;; - [c] in sauron :: clear notifies.

;; -- active --
;; Sauron shows those events like a list – basically like a log. You can
;; ‘activate’ an event by either pressing RET when point is on it, or clicking
;; it with the middle mouse button (<mouse-2>).

;; -- start & stop --
;; - [M-x sauron-start] -- start sauron.
;; - [M-x sauron-stop]  -- stop sauron.
;;
;; sauron-start will pop-up a new frame (window) which will show events coming
;; from any of its sources (i.e., ERC, org-mode appointments, D-Bus). You can
;; ‘activate’ a source by pressing “Enter” with the cursor on the event, which
;; will then take some backend-specific action.

;; -- clear --
;; - [c] / [M-x sauron-clear] -- clear event list.
;;
;; For example, for the ERC-backend, it will transfer you to the buffer
;; (IRC-channel) where said event happened. You can clear all events with M-x
;; sauron-clear (default keybinding: c).

;; -- toggle show/hide sauron frame --
;;
;; - [M-x sauron-toggle-hide-show] -- You can toggle between showing and hiding of the Sauron frame or window.

;; -- load modules --
;; Sauron (by default) loads the sauron-erc, sauron-org and sauron-dbus modules;
;; if you don’t have ERC, org-mode or d-bus support, these will simply be
;; ignored. If so desired, you can customize sauron-modules. See below for some
;; specifics about the backends.

;; -- backend modules --
;;
;; - erc
;; - org-mode (org-mode, appt)
;; - notifications
;; - d-bus
;; - identica
;; - twittering
;; - jabber


;;; Code:

;;; [ sauron ]

;; You should load org before starting sauron, in particular before you set
;; appt-disp-window-function, as sauron-org uses that same function (it will
;; preserve the existing functionality though).

;; sauron-org
(if (featurep 'org)
    (require 'org))
;; appointment
;; (if (featurep 'appt)
;;     (require 'appt))
;; sauron-erc
(if (featurep 'erc)
    (require 'erc))

(require 'sauron)

;; -- sauron display --
(setq sauron-separate-frame t) ; default t. nil to embeded in current frame.
(global-set-key [f8] 'sauron-toggle-hide-show)
(setq sauron-max-line-length 150)

;; -- sauron modeline --
(setq sauron-hide-mode-line t)      ; remove the mode-line in the sauron-buffer.

(setq sauron-sticky-frame t) ; make the Sauron window appears on every (virtual) desktop.

;; -- priorities --
(setq sauron-min-priority 3) ; ignore all events which priority is lower than this min-priority.

;; -- watching patterns --
(setq sauron-watch-patterns
      '("\\bhack\\b" "\\bcrack\\b" "\\bSex\\b")
      )

;; -- watching nicks --
(setq sauron-watch-nicks
      '("Tristan" "thuang" "yaxin"))
;; don’t get swamped by a certain nick
;;
;; Since you may not want to get too many events from one nick – and, who knows,
;; accompanying sound effects, pop-ups and what have you, you can set some
;; insensitivity time; events from the same nick during this time will be
;; lowered in priority by one point.
(setq sauron-nick-insensitivity 60)

;; -- blocking events from showing up --
;; (add-hook 'sauron-event-block-functions
;;           (lambda (origin priority msg &optional props)
;;             (or
;;              (string-match "foo" msg) ; ignore events that match 'foo'.
;;              ;; other matchers
;;              )))

;; -- doing stuff based on events --

;; -- seeing all events --
(setq sauron-log-events t) ; log all events in *Sauron Log* buffer.
(setq sauron-log-buffer-max-lines 500)

;;; some sound/light effects for certain events
;; TODO use some voice sound in Ingress game.
(add-hook 'sauron-event-added-functions
          (lambda (origin prio msg &optional props)
            (if (string-match "org" origin)
                (sauron-fx-aplay "/home/chris/Music/Sounds/Hacking Game/voice-incoming-transmission.wav"))
            (if (string-match "erc" origin)
                (sauron-fx-aplay "/home/chris/Music/Sounds/Hacking Game/hesfx-newmessage.wav"))
            (if (string-match "dbus" origin)
                (sauron-fx-aplay "/home/chris/Music/Sounds/Hacking Game/hesfx-newmessage.wav"))
            (cond
             ((= prio 3)
              (sauron-fx-aplay "/home/chris/Music/Sounds/Hacking Game/voice-incoming-transmission.wav"))
             ((= prio 4)
              (sauron-fx-aplay "/home/chris/Music/Sounds/Hacking Game/voice-incoming-transmission.wav"))
             ((= prio 5)
              (sauron-fx-aplay "/home/chris/Music/Sounds/Hacking Game/voice-please-confirm.wav")
              (sauron-fx-gnome-osd
               (format "%S: %s" origin msg) 5)))))

;;; events to ignore
(add-hook 'sauron-event-block-functions
          (lambda (origin prio msg &optional props)
            (or
             (string-match "^*** Users" msg)))) ;; filter out IRC spam



;;; [ alert.el ]
;; https://github.com/jwiegley/alert

;; TODO
;; (unless (package-installed-p 'altert)
;;   (package-install 'altert))
;; (require 'alert)
;; (add-hook ‘sauron-event-added-functions ‘sauron-alert-el-adapter)


;; -- backend modules --
;; - erc
;; - org-mode
;; - notifications (emacs24+)
;; - d-bus
;; - identica
;; - twittering
;; - jabber

(setq sauron-modules
      '(sauron-erc
        sauron-org
        sauron-dbus
        sauron-notifications
        ;; sauron-twittering sauron-identica
        ;; sauron-jabber
        ))


;;; [ Org-mode, appt ]
;; sauron-org (org-mode/appt)
;;
;; For org-mode, sauron adds functionality to appt-disp-window-function (but
;; leaves it intact), so that whenever some event is near, you get a
;; notification with the following priorities:
;;
;;    15 minutes left: priority 3
;;    10 minutes left: priority 3
;;    5 minutes left: priority 4
;;    2 minutes left: priority 5
;;
;; For all other minutes, you’ll get events with priority 2.

;; (require 'appt)
;; (setq appt-display-interval 3
;;       appt-message-warning-time 10
;;       appt-disp-window-function 'sr-org-handler-func)


;;; Sauron ERC
(setq sauron-erc-interesting-events '(privmsg current-nick keyword))


;;; [ D-Bus ]
(setq sauron-dbus-cookie t)






(provide 'init-my-tool-sauron)

;;; init-my-tool-sauron.el ends here
