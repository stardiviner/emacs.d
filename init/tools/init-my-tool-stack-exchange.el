;;; init-my-tool-stack-exchange.el --- init for Stack Exchange
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ SX ] -- Stack Exchange

;;; Usage:
;;
;; TODO:
;; - `sx-authenticate' :: authenticate your key. and store in ~/.emacs.d/.sx/auth.el
;; (setq browse-url-browser-function 'browse-url-firefox)
;; - `sx-tab-' functions prefix
;; - `sx-tab-all-questions'

;; View questions with one of the sx-tab- commands. These translate to the
;; different ‘tabs’ that you can view on the official site. Implemented tabs
;; include:
;;
;; - frontpage :: The default front page of questions.
;; - newest :: Newest questions first.
;; - topvoted :: Highest-voted questions first.
;; - hot :: Questions with the most views, answers, and votes over the last few days.
;; - week :: Questions with the most views, answers, and votes this week.
;; - month :: Questions with the most views, answers, and votes this month.
;;
;; The meaning of these tabs hopefully needs no explanation, but the official
;; behavior is given as a *tooltip* on any site in the StackExchange network.
;;
;; Each of these opens up a list of questions. Switch sites with `:'. Navigate
;; this list of questions with `jk' or `np'. `jk' will also view the question in
;; a separate buffer. `v' will visit the question in your browser where `w' will
;; simply copy a link. Upvote and downvote with `u' and `d'. `RET' will take you
;; to the question buffer, where `RET' on headlines will expand and collapse
;; each section. Add comments with `c'.
;;
;; As always, C-h m is the definitive resource for the functions of this mode.

(require 'sx-load)

;; question mode
(setq sx-question-mode-display-buffer-function 'pop-to-buffer
      ;; sx-question-mode-comments-format
      sx-question-mode-pretty-links t
      ;; sx-question-mode-separator "________________________________________________________________________________\n"
      sx-question-mode-recenter-line 2
      )

;; question list
(setq sx-question-list-height 12)

;; notify
;; (setq sx-notify-timer-delay 300)
;; (sx-notify-mode 1)


;; ;; user faces
;; (set-face-attribute 'sx-user-accept-rate nil
;;                     )
;; (set-face-attribute 'sx-user-name nil
;;                     )
;; (set-face-attribute 'sx-user-reputation nil
;;                     )
;; ;;; question list faces
;; (set-face-attribute 'sx-question-list-answers nil
;;                     )
;; (set-face-attribute 'sx-question-list-answers-accepted nil
;;                     )
;; (set-face-attribute 'sx-question-list-bounty nil
;;                     )
;; (set-face-attribute 'sx-question-list-date nil
;;                     )
;; (set-face-attribute 'sx-question-list-favorite nil
;;                     )
;; (set-face-attribute 'sx-question-list-parent nil
;;                     )
;; (set-face-attribute 'sx-question-list-read-question nil
;;                     )
;; (set-face-attribute 'sx-question-list-score nil
;;                     )
;; (set-face-attribute 'sx-question-list-score-upvoted nil
;;                     )
;; (set-face-attribute 'sx-question-list-tags nil
;;                     )
;; (set-face-attribute 'sx-question-list-unread-question nil
;;                     )
;; ;; question mode
;; (set-face-attribute 'sx-question-mode-content-face nil
;;                     )
;; (set-face-attribute 'sx-question-mode-date nil
;;                     )
;; (set-face-attribute 'sx-question-mode-header nil
;;                     )
;; (set-face-attribute 'sx-question-mode-score nil
;;                     )
;; (set-face-attribute 'sx-question-mode-score-upvoted nil
;;                     )
;; (set-face-attribute 'sx-question-mode-score-downvoted nil
;;                     )
;; (set-face-attribute 'sx-question-mode-tags nil
;;                     )
;; (set-face-attribute 'sx-question-mode-title nil
;;                     )
;; (set-face-attribute 'sx-question-mode-title-comments nil
;;                     )




(provide 'init-my-tool-stack-exchange)

;;; init-my-tool-stack-exchange.el ends here
