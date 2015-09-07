;;; init-my-prog-bug-track-system.el --- init for Bug Tracking System
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ bts ] -- bug track system

;;; This is a extension of Emacs that provides a unified interface for various
;;; bug tracking systems.
;; - This works using the plugin for target system.
;;
;;; Features:
;;   - Easily and quickly edit ticket of various bug tracking systems using a unified widget interface
;;   - List up ticket summaries with the combination of multiple conditions at one time
;;   - Edit the listed multiple tickets at one time

;;; Usage:
;;
;; - Setup project
;;
;; Project means a access configuration for the system stores target tickets data.
;; Create and update project using bts:project-new / bts:project-update.
;;
;; - Setup query
;;
;; Query means a configuration detects the fetched tickets in the tickets belongs to the project.
;; Create and update query using bts:query-new / bts:query-update.
;;
;; - Create new ticket
;;
;; Create a ticket of the system using bts:ticket-new.
;;
;; - Keymap of widget buffer
;;
;; Widget buffer is opened for editing project/query/ticket.
;; The following keymap is defined in the buffer inherits the standard widget keymap (*1).
;;
;; C-n bts:widget-forward … move to forward widget (*2)
;; C-p bts:widget-backward … move to backward widget (*2)
;; C-M-j bts:widget-jump … jump to any widget (*3)
;; M-RET bts:widget-submit … push submit button
;; M-DEL bts:widget-cancel … push cancel button
;;
;; *1 For checking that, see document of widget-keymap / widget-field-keymap / widget-text-keymap
;; *2 If possible, use pophint.el
;; *3 pophint.el is required
;;
;; List up tickets
;;
;; The system tickets are listed up by bts:summary-open.
;; Multiple queries are selectable in a choice of query.
;; Then, the summary buffer is opened inherits tabulated-list-mode.
;; The following keymap is defined in the buffer inherits tabulated-list-mode-map.
;;
;; j / n next-line … move to down entry
;; k / p previous-line … move to up entry
;; h / b bts:summary-left-column … move to left column
;; l / f bts:summary-right-column … move to right column
;; RET bts:summary-view-ticket … open ticket view (*1)
;; m bts:summary-mark-ticket … mark current entry
;; M bts:summary-mark-all-tickets … mark all entries
;; u bts:summary-unmark-ticket … unmark current entry
;; U / M-DEL bts:summary-unmark-all-tickets … unmark all entries
;; t bts:summary-toggle-ticket-marking … toggle current entry mark situation
;; T bts:summary-toggle-all-tickets-marking … toggle all entries mark situation
;; g / r bts:summary-reload-ticket … fetch the latest data and update current entry
;; G / R bts:summary-reload-all … fetch the latest datas and update all entries (*2)
;;
;; *1 If any entries are marked, target are them (*3). Else, it's current entry
;; *2 The buffer is not updated to latest automatically
;; *3 If target is multiple and the system has the function, open multi view (*4)
;; *4 Multi view is able to edit multiple tickets at one time

(require 'bts)

;; (require 'pophint)

(setq bts:preferred-selection-method 'helm
      bts:project-cache-file "~/.emacs.d/.bts/project"
      bts:query-cache-file "~/.emacs.d/.bts/query"
      bts:ticket-fetch-check-interval 3 ; seconds
      bts:ticket-multi-view-preferred t
      ;; widget label
      bts:widget-label-format " %s "
      bts:widget-label-prefix " "
      bts:widget-label-suffix " "
      bts:widget-require-mark "*"
      )


;; ;;; Faces
;; ;; summary
;; (set-face-attribute 'bts:summary-closed-ticket-face nil
;;                     )
;; (set-face-attribute 'bts:summary-condition-fetch-part-face nil
;;                     )
;; (set-face-attribute 'bts:summary-condition-grep-part-face nil
;;                     )
;; (set-face-attribute 'bts:summary-condition-marked-part-face nil
;;                     )
;; (set-face-attribute 'bts:summary-condition-query-part-face nil
;;                     )
;; (set-face-attribute 'bts:summary-condition-warn-part-face nil
;;                     )
;; (set-face-attribute 'bts:summary-ignored-ticket-face nil
;;                     )
;; (set-face-attribute 'bts:summary-mark-face nil
;;                     )
;; ;; ticket
;; (set-face-attribute 'bts:ticket-regist-message-failed-face nil
;;                     )
;; (set-face-attribute 'bts:ticket-regist-message-skipped-face nil
;;                     )
;; (set-face-attribute 'bts:ticket-regist-message-succeed-face nil
;;                     )
;; ;; widget
;; (set-face-attribute 'bts:widget-button-face nil
;;                     )
;; (set-face-attribute 'bts:widget-button-pressed-face nil
;;                     )
;; (set-face-attribute 'bts:widget-const-face nil
;;                     )
;; (set-face-attribute 'bts:widget-documentation-face nil
;;                     )
;; (set-face-attribute 'bts:widget-label-face nil
;;                     )
;; (set-face-attribute 'bts:widget-link-face nil
;;                     )
;; (set-face-attribute 'bts:widget-mouse-face nil
;;                     )
;; (set-face-attribute 'bts:widget-require-face nil
;;                     )
;; (set-face-attribute 'bts:widget-tip-face nil
;;                     )


;; Key Binding
(define-key my-prog-bug-bts-map (kbd "b") 'bts:summary-open)
(define-key my-prog-bug-bts-map (kbd "t") 'bts:ticket-new)
(define-key my-prog-bug-bts-map (kbd "P") 'bts:project-new)
(define-key my-prog-bug-bts-map (kbd "p") 'bts:project-update)
(define-key my-prog-bug-bts-map (kbd "r") 'bts:project-remove)
(define-key my-prog-bug-bts-map (kbd "R") 'bts:project-remove-all)
(define-key my-prog-bug-bts-map (kbd "q") 'bts:query-new)
(define-key my-prog-bug-bts-map (kbd "Q") 'bts:query-update)
(define-key my-prog-bug-bts-map (kbd "d") 'bts:query-remove)
(define-key my-prog-bug-bts-map (kbd "D") 'bts:query-remove-all)


;; TODO:
;; About other config item, see Customization or eval the following sexp.
;; (customize-group "bts")




;; [ bts-github ] -- bts for GitHub

;;; Usage:
;;
;; -

(require 'bts-github)

(setq bts-github:ignore-labels '("duplicate" "invalid" "wontfix")
      ;; bts-github:max-lisp-eval-depth 6000
      ;; bts-github:max-specpdl-size 13000
      bts-github:summary-id-width 4
      bts-github:summary-label-decorating t ; Whether to decorate issue labels column.
      bts-github:summary-label-width 15
      )

;;; Faces
(set-face-attribute 'bts-github:issue-comment-header-face nil
                    )
(set-face-attribute 'bts-github:summary-label-decorating nil
                    )
(set-face-attribute 'bts-github:summary-label-face nil
                    )

;; TODO:
;; About config item, see Customization or eval the following sexp.
;; (customize-group "bts-github")



(provide 'init-my-prog-bug-track-system)

;;; init-my-prog-bug-track-system.el ends here
