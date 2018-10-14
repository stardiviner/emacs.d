;;; init-emacs-backup.el --- init for Emacs backup

;;; Commentary:

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Backup.html

;;; Usage:

;; - `recover-file' :: recover file from file~.


;;; Code:


;; .#foo.txt
;; modify same file by different processes
(setq create-lockfiles t)
;; foo.txt~
;; backup the latest recent file
(setq make-backup-files t)
;; #foo.txt#
;; periodly save, otherwise lost data when accidently power-off.
(setq auto-save-default t)


(setq backup-by-copying t
      backup-by-copying-when-mismatch t
      backup-by-copying-when-privileged-mismatch t
      backup-by-copying-when-linked t
      version-control t ; use versioned backups filename.ext.~1~
      vc-make-backup-files t ; do not backup files in vc.
      ;; backup-inhibited t ; do not generate backup
      delete-old-versions t             ; auto delete old versions.
      kept-new-versions 3               ; number of new versions.
      kept-old-versions 3               ; number of old versions.
      )

(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/.backups")))
      ;; `((".*" . ,temporary-file-directory)) ; put all under directory /tmp.
      )


;;; [ auto-save-mode ] -- toggle auto-saving in the current buffer.

;;; From time to time, Emacs automatically saves each visited file in a separate file,
;;; without altering the file you actually use. This is called “auto-saving”.

(setq auto-save-default t               ; create #autosave# files
      auto-save-list-file-prefix "~/.emacs.d/.auto-save-list/saves-"
      auto-save-interval (* 60 10)
      ;; delete-auto-save-files nil ; don't delete auto-save file when a buffer is saved or killed.
      )
;; (auto-save-mode 1)

;;; [ auto-save-visited-mode ]

;; Toggle automatic saving to file-visiting buffers on or off.
;; With a prefix argument ARG, enable regular saving of all buffers visiting a file if ARG
;; is positive, and disable it otherwise. Unlike `auto-save-mode', this mode will
;; auto-save buffer contents to the visited files directly and will also run all
;; save-related hooks. See Info node `Saving' for details of the save process.

(auto-save-visited-mode t)

;;; [ auto-save ]

;;; https://www.emacswiki.org/emacs/auto-save.el
;;; https://www.emacswiki.org/emacs/init-auto-save.el
;;; ;; 禁用Emacs备份机制
;;; (setq make-backup-files nil)
;;; (setq auto-save-default nil)
;;; ;; 启用 LazyCat 的自动备份插件
;;; (require 'auto-save)
;;; (auto-save-enable)
;;; (setq auto-save-slient t)
;;; (setq auto-save-delete-trailing-whitespace t)


(provide 'init-emacs-backup)

;;; init-emacs-backup.el ends here
