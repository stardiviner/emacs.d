;;; init-my-tool-irc-weechat.el --- init for IRC WeeChat.

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ weechat.el ] -- Chat via weechat's relay protocol in Emacs.

(use-package weechat
  :ensure t
  :config
  (setq weechat-modules '(weechat-button
                          weechat-complete
                          weechat-cmd
                          weechat-spelling
                          weechat-notifications
                          weechat-tracking
                          weechat-smiley
                          weechat-latex
                          weechat-image
                          ;; weechat-speedbar
                          ))

  ;; Notification
  (setq weechat-notification-types '(:highlight :disconnect :query))

  ;; Tracking
  (setq weechat-tracking-types '(:highlight))

  ;; Spelling
  ;; (setq weechat-spelling-dictionaries )
  
  ;; (when (WeeChat process is running)
  ;;   (weechat-connect "localhost" "9000" "PASSWORD")
  ;;   ;; (weechat-monitor-buffer)
  ;;   )
  )

;;; ----------------------------------------------------------------------------

(provide 'init-my-tool-irc-weechat)

;;; init-my-tool-irc-weechat.el ends here
