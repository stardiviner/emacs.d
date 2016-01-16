;;; init-my-prog-sidebar.el --- init Emacs sidebar for Programming.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


;; (require 'init-my-prog-ecb)
;; (require 'init-my-prog-speedbar)
;; (require 'init-my-prog-neotree)
(require 'init-my-prog-project-explorer)



;; (global-set-key [f8] 'sr-speedbar-toggle)
;; (global-set-key [f8] 'my-sr-speedbar-toggle-and-switch)

(global-set-key [f8] 'project-explorer-toggle)


(provide 'init-my-prog-sidebar)

;;; init-my-prog-sidebar.el ends here
