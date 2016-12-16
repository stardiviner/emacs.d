;;; init-my-emacs-backup.el --- init for Emacs backup

;;; Commentary:

;;; Usage:

;; - `recover-file' :: recover file from file~.


;;; Code:
;;; ----------------------------------------------------------------------------

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


;;; [ auto save ]

(setq auto-save-default t               ; create #autosave# files
      auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-"
      auto-save-interval 1500)

;;; auto-save visited files
;; Non-nil says auto-save a buffer in the file it is visiting, when practical.
(setq auto-save-visited-file-name t)

;; (auto-save-mode t)

;;; ----------------------------------------------------------------------------

(provide 'init-my-emacs-backup)

;;; init-my-emacs-backup.el ends here
