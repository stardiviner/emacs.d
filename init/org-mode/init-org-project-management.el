;;; init-org-project-management.el --- Project Management with Org Mode.

;;; Time-stamp: <2019-06-24 20:58:17 stardiviner>

;;; Commentary:



;;; Code:

;;; [ org-gantt ] -- Generate Gantt chart for Org Mode headlines.

(leaf org-gantt
  :straight (org-gantt :type git :host github :repo "stardiviner/org-gantt" :files ("org-gantt.el")))



(provide 'init-org-project-management)

;;; init-org-project-management.el ends here
