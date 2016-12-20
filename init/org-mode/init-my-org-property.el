;;; init-my-org-property.el --- init for Org Properties
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;; (setq org-use-property-inheritance t)

;;; auto adding CUSTOM_ID property for heading
;;
;; (require 'org-id)
;; (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;;; add org-babel header-args property into default properties list.
(add-to-list 'org-default-properties "header-args")

(add-to-list 'org-default-properties "DATE")
(add-to-list 'org-default-properties "URL")

(provide 'init-my-org-property)

;;; init-my-org-property.el ends here
