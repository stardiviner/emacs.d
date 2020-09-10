;;; init-org-project-management.el --- Project Management with Org Mode.

;;; Time-stamp: <2020-09-11 05:48:14 stardiviner>

;;; Commentary:



;;; Code:

;;; [ org-gantt ] -- Generate Gantt chart for Org Mode headlines.

(use-package org-gantt
  :quelpa (org-gantt :fetcher github :repo "stardiviner/org-gantt")
  :defer t
  :commands (org-insert-dblock:org-gantt-chart))

;;; [ elgantt ] -- A Gantt Chart (Calendar) for Org Mode.

(use-package elgantt
  :quelpa (elgantt :fetcher github :repo "legalnonsense/elgantt")
  :defer t
  :commands (elgantt-open)
  :custom (elgantt-agenda-files (concat org-directory "Projects/Gantt/Gantt.org")))



(provide 'init-org-project-management)

;;; init-org-project-management.el ends here
