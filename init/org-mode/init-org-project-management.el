;;; init-org-project-management.el --- Project Management with Org Mode.

;;; Time-stamp: <2018-11-16 08:11:03 stardiviner>

;;; Commentary:



;;; Code:

;;; [ org-gantt ] -- Generate Gantt chart for Org Mode headlines.

(use-package org-gantt
  :quelpa (org-gantt :fetcher github :repo "swillner/org-gantt" :files ("org-gantt.el"))
  :init
  (defun org-insert-dblock:org-gantt ()
    "Insert org-gantt dynamic block."
    (interactive)
    (org-create-dblock
     (list :name "org-gantt"
           :file "data/images/project-gantt-chart.png"
           :imagemagick t
           :tikz-options "scale=1.5, every node/.style={scale=1.5}"
           :weekend-style "{draw=blue!10, line width=1pt}"
           :workday-style "{draw=blue!5, line width=.75pt}"
           :show-progress 'if-value
           :progress-source 'cookie-clocksum
           :no-date-headlines 'inactive
           :parameters "y unit title=.7cm, y unit chart=.9cm"
           :tags-group-style '(("test"."group label font=\\color{blue}")
                               ("toast"."group label font=\\color{green}"))
           :tags-bar-style '(("test"."bar label font=\\color{blue}")
                             ("toast"."bar label font=\\color{green}"))))))



(provide 'init-org-project-management)

;;; init-org-project-management.el ends here
