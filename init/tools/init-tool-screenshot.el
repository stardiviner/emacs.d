;;; init-tool-screenshot.el --- init for Screenshot
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'screenshot-prefix)
  (define-prefix-command 'screenshot-prefix))
(define-key tools-prefix (kbd "S") 'screenshot-prefix)

;;; Emacs 27.1 can use Cairo to take SVG screenshots of itself.
(defun screenshot-take (&optional type)
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive (list (completing-read "Output Type: "
                                      '("svg" "png" "postscript" "pdf"))))
  (let* ((type (or type "png"))
         (filename (expand-file-name (format "~/Desktop/%s.%s" (make-temp-name "Emacs_Screenshot_") type)))
         (data (x-export-frames nil (intern type))))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

(use-package frameshot
  :ensure t
  :commands (frameshot-mode frameshot-take)
  :bind (:map screenshot-prefix ("S" . frameshot-mode)))


(provide 'init-tool-screenshot)

;;; init-tool-screenshot.el ends here
