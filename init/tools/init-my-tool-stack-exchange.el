;;; init-my-tool-stack-exchange.el --- init for Stack Exchange
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ SX ] -- Stack Exchange

;;; Usage:
;;
;; - after install, auth if you want to vote or comment, answer etc.
;; - `sx-authenticate' :: authenticate your key. store in ~/.emacs.d/.sx/auth.el
;;
;; - `sx-tab-' functions prefix
;; - `sx-tab-all-questions'

(require 'sx-load)

;; question mode
(setq sx-question-mode-display-buffer-function #'pop-to-buffer
      ;; sx-question-mode-comments-format
      sx-question-mode-pretty-links t
      ;; sx-question-mode-separator ""
      sx-question-mode-recenter-line 2
      )

;; notify
(setq sx-notify-timer-delay (* 60 10))
(sx-notify-mode 1)


;; user faces
(set-face-attribute 'sx-user-accept-rate nil
                    )
(set-face-attribute 'sx-user-name nil
                    )
(set-face-attribute 'sx-user-reputation nil
                    )
;;; question list faces
(set-face-attribute 'sx-question-list-answers nil
                    :foreground "forest green")
(set-face-attribute 'sx-question-list-answers-accepted nil
                    :foreground "green")
(set-face-attribute 'sx-question-list-bounty nil
                    :foreground "red3")
(set-face-attribute 'sx-question-list-date nil
                    )
(set-face-attribute 'sx-question-list-parent nil
                    )
(set-face-attribute 'sx-question-list-read-question nil
                    :foreground "dim gray")
(set-face-attribute 'sx-question-list-unread-question nil
                    :foreground "gray")
(set-face-attribute 'sx-question-list-favorite nil
                    )
(set-face-attribute 'sx-question-list-score nil
                    :foreground "dark gray")
(set-face-attribute 'sx-question-list-score-upvoted nil
                    :foreground "blue" :weight 'bold)
(set-face-attribute 'sx-question-list-tags nil
                    )
;; question mode
(set-face-attribute 'sx-question-mode-content-face nil
                    :background (color-darken-name (face-background 'default) 3))
(set-face-attribute 'sx-question-mode-date nil
                    )
(set-face-attribute 'sx-question-mode-header nil
                    )
(set-face-attribute 'sx-question-mode-score nil
                    )
(set-face-attribute 'sx-question-mode-score-upvoted nil
                    :foreground "yellow")
(set-face-attribute 'sx-question-mode-score-downvoted nil
                    :foreground "dark magenta")
(set-face-attribute 'sx-question-mode-tags nil
                    )
(set-face-attribute 'sx-question-mode-title nil
                    )
(set-face-attribute 'sx-question-mode-title-comments nil
                    )




(provide 'init-my-tool-stack-exchange)

;;; init-my-tool-stack-exchange.el ends here
