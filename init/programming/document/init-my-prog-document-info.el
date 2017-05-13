;;; init-my-prog-document-info.el --- init for Info
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Info ]

(global-set-key (kbd "C-h i") 'info-display-manual)

(defun info-display-manual-in-buffer (topic)
  "Display Info TOPIC in its own buffer."
  (interactive
   (list
    (progn
      (info-initialize)
      (completing-read "Info Manual name: "
                       (info--manual-names)
                       nil t))))
  (let ((bufname (format "*info: %s*" topic)))
    (if (get-buffer bufname)
        (switch-to-buffer bufname)
      (info topic bufname))))

(use-package info+
  :ensure t
  :defer t
  :config
  (setq Info-breadcrumbs-in-header-flag t
        ;; Info-display-node-header-fn
        Info-fit-frame-flag t
        Info-fontify-angle-bracketed-flag t
        Info-fontify-quotations-flag t
        Info-fontify-reference-items-flag t
        Info-fontify-single-quote-flag t
        Info-saved-nodes t
        )
  )

(use-package niceify-info
  :ensure t
  :defer t
  :init
  (add-hook 'Info-selection-hook #'niceify-info))


(provide 'init-my-prog-document-info)

;;; init-my-prog-document-info.el ends here
