;;; init-my-org-mobile.el --- init for MobileOrg.

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
;;; [ org-mobile ]

(require 'org-mobile)

(let ((org-mobile-directory (concat org-directory "/Tasks/MobileOrg"))
      (org-mobile-dropbox-directory (concat (getenv "HOME") "/Dropbox/Apps/MobileOrg")))
  (if (and (file-exists-p org-mobile-directory)
           (file-symlink-p org-mobile-dropbox-directory))
      (setq org-mobile-directory (concat org-directory "/Tasks/MobileOrg/"))
    (make-directory org-mobile-directory)
    (make-directory org-mobile-dropbox-directory)
    ))

(setq org-mobile-files (org-agenda-files)
      org-mobile-inbox-for-pull (concat org-mobile-directory "/from-mobile.org")
      )




;;; ----------------------------------------------------------------------------

(provide 'init-my-org-mobile)

;;; init-my-org-mobile.el ends here
