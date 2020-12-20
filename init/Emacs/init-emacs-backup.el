;;; init-emacs-backup.el --- init for Emacs backup

;;; Commentary:

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Backup.html

;;; Usage:

;; - `recover-file' :: recover file from backup file suffix with "file~".


;;; Code:

;;; [ backup ]
;; .#foo.txt
;; modify same file by different processes
(setq create-lockfiles t)
;; foo.txt~
;; backup the latest recent file
(setq make-backup-files t)

(setq backup-by-copying t
      backup-by-copying-when-mismatch t
      backup-by-copying-when-privileged-mismatch t
      backup-by-copying-when-linked t)

;; (setq backup-directory-alist
;;       '(("." . ".emacs_backups")) ; save backups in $(pwd)/.emacs_backups/filename.bak
;;       ;; `(("." . ,(expand-file-name (expand-file-name ".backups" user-emacs-directory))))
;;       ;; `((".*" . ,temporary-file-directory)) ; put all under directory /tmp.
;;       )

;;; [ version control ]
;;; version control with versioned backup filename like "filename.ext.~1~".

(setq version-control nil               ; toggle version control backup?
      vc-make-backup-files nil          ; do not backup files in vc.
      ;; backup-inhibited t                ; do not generate backup
      delete-old-versions t             ; auto delete old versions.
      kept-new-versions 2               ; number of new versions.
      kept-old-versions 2               ; number of old versions.
      )

;;; [ auto-save-mode ] -- toggle auto-saving in the current buffer.

;;; From time to time, Emacs automatically saves each visited file in a separate file,
;;; without altering the file you actually use. This is called “auto-saving”.
;;
;; create filename with "#foo.txt#".
;; periodly save, otherwise lost data when accidently power-off.

(setq auto-save-list-file-prefix (expand-file-name ".auto-save-list/.saves-" user-emacs-directory)
      auto-save-interval (* 60 10)
      ;; delete-auto-save-files nil ; don't delete auto-save file when a buffer is saved or killed.
      auto-save-no-message t
      )
(auto-save-mode 1)

;;; [ auto-save-visited-mode ]

;; Toggle automatic saving to file-visiting buffers on or off.
;; With a prefix argument ARG, enable regular saving of all buffers visiting a file if ARG
;; is positive, and disable it otherwise. Unlike `auto-save-mode', this mode will
;; auto-save buffer contents to the visited files directly and will also run all
;; save-related hooks. See Info node `Saving' for details of the save process.

(auto-save-visited-mode t)

;;; [ super-save ] -- Auto-save buffers, based on your activity, Save Emacs buffers when they lose focus.

;; (use-package super-save
;;   :ensure t
;;   :defer t
;;   :init (setq auto-save-default nil ; turn off Emacs built-in `auto-save-mode'.
;;               super-save-auto-save-when-idle t
;;               super-save-remote-files nil)
;;   (super-save-mode 1)
;;   :config
;;   ;; add integration with ace-window
;;   (add-to-list 'super-save-triggers 'ace-window)
;;   ;; save on find-file
;;   (add-to-list 'super-save-hook-triggers 'find-file-hook)
;;   (setq super-save-exclude `(
;;                              ,(rx "*\\.pdf")
;;                              ;; mu4e compose buffer
;;                              ,(rx "*draft*")
;;                              ,(rx bol
;;                                   (eval (expand-file-name "Mails/Drafts/cur/*" "~"))
;;                                   eol)))
;;
;;   ;; workarounds for some modes.
;;   ;; disable `super-save' in `mu4e-compose-mode'.
;;   (add-hook 'mu4e-compose-mode-hook #'(lambda ()) (if (fboundp 'super-save-stop) (super-save-stop)))
;;   )


(provide 'init-emacs-backup)

;;; init-emacs-backup.el ends here
