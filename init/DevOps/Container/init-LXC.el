;;; init-LXC.el --- init for LXC

;;; Time-stamp: <2019-01-18 09:26:12 stardiviner>

;;; Commentary:



;;; Code:


;;; [ LXC ] -- Linux Containers (LXC) integration with Emacs.

(use-package lxc
  :ensure t
  :commands (list-lxc))

;;; [ lxc-tramp ] -- TRAMP integration for LXC containers.

(use-package lxc-tramp
  :ensure t)

;;; [ LXD ]

(use-package lxd-tramp ; TRAMP integration for LXD containers.
  :ensure t)



(provide 'init-LXC)

;;; init-LXC.el ends here
