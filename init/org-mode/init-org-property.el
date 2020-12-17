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
(add-to-list 'org-default-properties "SCORE")

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
(add-to-list 'org-default-properties "PUBLISHER(China)")
(add-to-list 'org-default-properties "PRESS")
(add-to-list 'org-default-properties "Series")
(add-to-list 'org-default-properties "YEAR")

(add-to-list 'org-default-properties "IMDb")
(add-to-list 'org-default-properties "ISBN")
(add-to-list 'org-default-properties "Douban")

;;===============================================================================
;;; auto evaluate inline source block in property "EVAL".

(defcustom org-property-eval-keywords-list '("EVAL")
  "A list of property keywords for evaluate code."
  :type 'list
  :safe #'listp
  :group 'org)

(dolist (prop org-property-eval-keywords-list)
  (add-to-list 'org-default-properties prop))

(defun org-property-eval-code (&optional state)
  "Evaluate Org inline source block in property value."
  (when (memq state '(children subtree))
    ;; TODO: detect property keywords in `org-property-eval-keywords-list'.
    ;; (require 'seq nil t)
    ;; (seq-intersection ... org-property-eval-keywords-list)
    (if-let ((inline-src-block (org-entry-get nil "EVAL" nil)))
        (with-temp-buffer
          (insert inline-src-block)
          (goto-char (point-min))
          (require 'ob-async nil t)
          (setq-local org-babel-default-inline-header-args
                      '((:results . "silent") (:async . t)))
          (let* ((context (org-element-context))
                 (src-block-info (org-babel-get-src-block-info nil context))
                 (type (org-element-type context)))
            (when (eq type 'inline-src-block)
              ;; ob-async: advice `org-babel-execute-src-block:async' on ‘org-babel-execute-src-block’
              (org-babel-execute-src-block nil src-block-info)))))))

(add-hook 'org-cycle-hook #'org-property-eval-code)


(provide 'init-org-property)

;;; init-org-property.el ends here
