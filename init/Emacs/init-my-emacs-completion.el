;;; init-my-emacs-completion.el --- my Emacs completion frameworks init

;;; Commentary:


;;; Code:

;;; [ complete ]

;;; Usage:
;;
;; - `completion-at-point-functions' is a special hook.
;;    (add-hook 'completion-at-point-functions 'completion-function)

;; (setq-default completion-at-point-functions
;;               '(pcomplete-completions-at-point
;;                 semantic-completion-at-point-function
;;                 tags-completion-at-point-function))

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


;;; [ Icomplete (icomplete/ido/iswitchb) ] -- enhance the default minibuffer completion.

;;; Usage:
;;
;; - [M-x icomplete-mode] :: toggle `icomplete-mode'.
;; - See `icomplete-completions' docstring for a description of the icomplete
;;   display format.


;; (require 'icomplete)
;;
;; ;; (add-hook 'icomplete-minibuffer-setup-hook
;; ;;           ())
;;
;; (icomplete-mode 1)
;;
;; (setq icomplete-compute-delay 0.3
;;       icomplete-max-delay-chars 2
;;       icomplete-in-buffer nil ; also use Icomplete when completing in non-mini buffers.
;;       icomplete-separator " | "
;;       icomplete-hide-common-prefix t
;;       icomplete-show-matches-on-no-input nil
;;       )
;;
;; (set-face-attribute 'icomplete-first-match nil
;;                     :weight 'bold)


;;; Press [TAB] in minibuffer to show completions in popup window buffer.




(require 'init-helm)
;; (require 'init-ido)
(require 'init-ivy)



(provide 'init-my-emacs-completion)

;;; init-my-emacs-completion.el ends here
