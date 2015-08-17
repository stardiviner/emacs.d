;;; init-my-emacs-buffer.el --- init Emacs buffer settings.

;;; Commentary:



;;; Code:

;;; Auto-reload external file changes

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)


;;; [ ibuffer ]

;;; Usage:
;;
;; - (M-x ibuffer-mode)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; [ ibuffer-vc ]

;; - Group your buffers by their parent vc root directory
;; - See the VC status of the associated files
;; - Sort buffers by their VC status

;;; Usage:
;;
;; - [M-x ibuffer-vc]

;; (require 'ibuffer-vc)


;;; [ ibuffer-git ]


;;; [ ibuffer-tramp ]


;;; [ ibuffer-projectile ]




(setq buffers-menu-max-size 30)


;;; [uniquify] -- meaningful names for buffers with the same name

(require 'uniquify)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*" ; don't muck with special buffers
      )


;;; [ recentf ]

(require 'recentf)

(recentf-mode)

(setq recentf-save-file (expand-file-name ".temp/recentf" user-emacs-directory)
      recentf-exclude '("/tmp/" "/ssh:")
      recentf-max-menu-items 25
      recentf-max-saved-items 100
      recentf-auto-cleanup 'mode ; 'mode, 'never.
      recentf-case-fold-search t
      )



(provide 'init-my-emacs-buffer)

;;; init-my-emacs-buffer.el ends here
