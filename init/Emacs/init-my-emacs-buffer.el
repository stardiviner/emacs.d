;;; init-my-emacs-buffer.el --- init Emacs buffer settings.

;;; Commentary:



;;; Code:

;;; Auto-reload external file changes

;; - `recover-file' :: recover file.
;; - `recover-session' :: 

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)


;;; auto-save visited files

;; Non-nil says auto-save a buffer in the file it is visiting, when practical.
(setq auto-save-visited-file-name t)


;;; [ auto save ]

;; (auto-save-mode t)
(setq auto-save-default t               ; create #autosave# files
      auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-"
      auto-save-interval 1500)


;;; [ backup ]

(setq make-backup-files t
      backup-by-copying t
      backup-by-copying-when-mismatch t
      backup-by-copying-when-privileged-mismatch t
      backup-by-copying-when-linked t
      version-control t ; use versioned backups.
      vc-make-backup-files nil ; do not backup files in vc.
      ;; backup-inhibited ; do not generate backup
      delete-old-versions t             ; auto delete old versions.
      kept-new-versions 3               ; number of new versions.
      kept-old-versions 3               ; number of old versions.
      version-control t                 ; multiple versions backup.
      )

(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/.backups")))
      ;; `((".*" . ,temporary-file-directory)) ; put all under directory /tmp.
      )


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
