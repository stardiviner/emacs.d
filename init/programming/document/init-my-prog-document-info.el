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


(provide 'init-my-prog-document-info)

;;; init-my-prog-document-info.el ends here
