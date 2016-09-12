;;; init-my-tool-ascii.el --- init for ASCII
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------

(defun my-figlet-region (&optional b e)
  "Region select text, then execute command [M-x my-figlet-region]."
  (interactive "r")
  (shell-command-on-region b e "toilet" (current-buffer) t))



;;; ----------------------------------------------------------------------------

(provide 'init-my-tool-ascii)

;;; init-my-tool-ascii.el ends here
