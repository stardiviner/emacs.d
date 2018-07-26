;;; init-lxc.el --- Linux Containers (LXC) init file.

;;; Time-stamp: <2018-07-31 09:30:19 stardiviner>

;;; Commentary:



;;; Code:


(use-package lxc
  :ensure t
  :defer t)

(use-package lxc-tramp ; Tramp integration for LXC containers.
  :ensure t
  :defer t)



(provide 'init-lxc)

;;; init-lxc.el ends here
