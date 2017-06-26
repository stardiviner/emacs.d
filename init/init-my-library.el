;;; init-my-library.el --- some small utils / libraries for easy usage.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; get the content of region.

(defun region-content ()
  "Get the region content if region is active."
  (interactive)
  (if (use-region-p) ; `region-active-p'
      (buffer-substring-no-properties (region-beginning) (region-end))
    ))


;;; [ hierarchy ] -- Emacs library to create, query, navigate and display hierarchy structures.

(use-package hierarchy
  :ensure t)



(provide 'init-my-library)

;;; init-my-library.el ends here
