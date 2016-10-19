;;; init-my-org-functions.el --- init for Org-mode helper functions.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; auto replace ’ with '

(defun my/org-mode-buffer-replace-curve-quote-with-straight ()
  "Replace ’ with ' in org-mode buffer after paste from other places."
  (interactive)
  (save-excursion
    (when (and (eq major-mode 'org-mode)
               ;; TODO: the `BOUND' is not working. always return nil.
               (isearch-search-string "’" (point-min) t))
      (goto-char (point-min))
      (query-replace "’" "'")
      (query-replace "“" "'")
      (query-replace "”" "'")
      )))

;; (add-hook 'after-save-hook 'my/org-mode-buffer-replace-curve-quote-with-straight)



(provide 'init-my-org-functions)

;;; init-my-org-functions.el ends here
