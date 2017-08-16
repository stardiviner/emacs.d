;;; init-my-tool-contacts.el --- init for Contacts Manager

;;; Commentary:



;;; Code:

;;; [ EBDB ] -- EBDB is an EIEIO port of BBDB, contact management/addressbook package for Emacs.

(use-package ebdb
  :ensure t
  :commands (ebdb)
  :config
  (setq ebdb-sources "~/.emacs.d/ebdb")
  
  ;; (use-package ebdb-i18n-chn
  ;;   :ensure t)
  ;; (use-package ebdb-gnorb
  ;;   :ensure t)
  (use-package company-ebdb
    :ensure t)
  (use-package counsel-ebdb
    :ensure t)
  (use-package helm-ebdb
    :ensure t)
  )



(provide 'init-my-tool-contacts)

;;; init-my-tool-contacts.el ends here
