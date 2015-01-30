;;; init-my-tool-stack-exchange.el --- init for Stack Exchange
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ SX ] -- Stack Exchange

;;; Usage:
;;
;; - `sx-tab-' functions ::
;; - `sx-authenticate' :: authenticate your key. and store in ~/.emacs.d/.sx/auth.el

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




(provide 'init-my-tool-stack-exchange)

;;; init-my-tool-stack-exchange.el ends here
