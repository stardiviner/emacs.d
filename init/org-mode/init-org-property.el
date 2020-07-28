;;; init-org-property.el --- init for Org Properties
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; (setq org-use-property-inheritance t)

(add-to-list 'org-default-properties "LOCATION")
(add-to-list 'org-default-properties "CLASS")

(add-to-list 'org-default-properties "COPYRIGHT")
(add-to-list 'org-default-properties "DATE")
(add-to-list 'org-default-properties "TIME")
(add-to-list 'org-default-properties "ALIAS")
(add-to-list 'org-default-properties "SOURCE")
(add-to-list 'org-default-properties "AUTHOR")
(add-to-list 'org-default-properties "EMAIL")
(add-to-list 'org-default-properties "URL")

(add-to-list 'org-default-properties "Source-Code")
(add-to-list 'org-default-properties "ISSUE")
(add-to-list 'org-default-properties "Pull-Request")
(add-to-list 'org-default-properties "COMMIT")
(add-to-list 'org-default-properties "CONFIG")
(add-to-list 'org-default-properties "Translation-Chinese")
(add-to-list 'org-default-properties "Translator")
(add-to-list 'org-default-properties "Programming-Language")
(add-to-list 'org-default-properties "PAPER")
(add-to-list 'org-default-properties "PUBLISHER")
(add-to-list 'org-default-properties "PRESS")

(add-to-list 'org-default-properties "IMDb")
(add-to-list 'org-default-properties "ISBN")
(add-to-list 'org-default-properties "Douban")


(provide 'init-org-property)

;;; init-org-property.el ends here
