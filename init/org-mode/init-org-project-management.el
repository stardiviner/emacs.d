;;; init-org-project-management.el --- Project Management with Org Mode.

;;; Time-stamp: <2020-01-23 17:21:12 stardiviner>

;;; Commentary:



;;; Code:

;;; [ org-gantt ] -- Generate Gantt chart for Org Mode headlines.

(use-package org-gantt
  :quelpa (org-gantt :fetcher github :repo "stardiviner/org-gantt")
  :commands (org-insert-dblock:org-gantt-chart))



(provide 'init-org-project-management)

;;; init-org-project-management.el ends here
