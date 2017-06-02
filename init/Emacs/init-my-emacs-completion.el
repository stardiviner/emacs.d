;;; init-my-emacs-completion.el --- my Emacs completion frameworks init

;;; Commentary:


;;; Code:

;;; [ complete ]

;;; Usage:
;;
;; - `completion-at-point-functions' is a special hook.
;;    add a completion command into it with mode locally.
;;    (add-hook 'completion-at-point-functions 'completion-function nil t)

(setq completion-ignore-case t)

(setq-default completion-at-point-functions nil)

;; (setq-default completion-at-point-functions
;;               '(pcomplete-completions-at-point
;;                 ;; semantic-completion-at-point-function
;;                 ;; tags-completion-at-point-function
;;                 ))

;; (setq tab-always-indent 'complete)
;; (add-to-list 'completion-styles 'initials t)
;; (setq completion-cycle-threshold 5)


(set-face-attribute 'completions-common-part nil
                    :foreground "forest green")
(set-face-attribute 'completions-first-difference nil
                    :foreground "salmon"
                    :weight 'bold)
(set-face-attribute 'completions-annotations nil
                    :inherit 'italic
                    :foreground "dark gray"
                    )


;;; [ pcomplete ] --- Programmable, Context-Sensitive Completion Library

(load-library "pcomplete")

(setq pcomplete-ignore-case t)


(require 'init-helm)
(require 'init-ivy)



(provide 'init-my-emacs-completion)

;;; init-my-emacs-completion.el ends here
