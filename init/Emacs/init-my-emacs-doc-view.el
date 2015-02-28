;;; init-my-emacs-doc-view.el --- init for doc-view.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ doc-view ]

;;; Usage:
;;
;; - [] ::

(require 'doc-view)

(setq doc-view-continuous t
      ;; doc-view--image-type ; 'png, 'tiff
      ;; doc-view-doc-type ; Can be `dvi', `pdf', or `ps'.
      ;; doc-view-cache-directory "/tmp/docview1000"
      doc-view-scale-internally t
      doc-view-image-width 850
      )



(provide 'init-my-emacs-doc-view)

;;; init-my-emacs-doc-view.el ends here
