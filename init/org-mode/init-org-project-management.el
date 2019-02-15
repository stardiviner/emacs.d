;;; init-org-project-management.el --- Project Management with Org Mode.

;;; Time-stamp: <2019-02-15 15:37:49 stardiviner>

;;; Commentary:



;;; Code:

;;; [ org-gantt ] -- Generate Gantt chart for Org Mode headlines.

(use-package org-gantt
  ;; :quelpa (org-gantt :fetcher github :repo "swillner/org-gantt" :files ("org-gantt.el"))
  :quelpa (org-gantt :fetcher github :repo "stardiviner/org-gantt" :files ("org-gantt.el")))



(provide 'init-org-project-management)

;;; init-org-project-management.el ends here
