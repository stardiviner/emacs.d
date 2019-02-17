;;; init-org-project-management.el --- Project Management with Org Mode.

;;; Time-stamp: <2019-02-18 14:13:35 stardiviner>

;;; Commentary:



;;; Code:

;;; [ org-gantt ] -- Generate Gantt chart for Org Mode headlines.

(use-package org-gantt
  ;; :quelpa (org-gantt :fetcher github :repo "swillner/org-gantt" :files ("org-gantt.el"))
  :quelpa (org-gantt :fetcher github :repo "stardiviner/org-gantt" :files ("org-gantt.el"))
  :defer t)



(provide 'init-org-project-management)

;;; init-org-project-management.el ends here
