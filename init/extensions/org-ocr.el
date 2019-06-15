;;; org-ocr.el --- Recognize the text on images and insert it in place as a quote block in Org-mode -*- lexical-binding: t; -*-

;;; Code:

(require 'request)

(defvar org-ocr-api-url
  "http://127.0.0.1:8080/ocr"
  "URL of the OCR service provider. For instructions on setting up the local
OCR server, see https://github.com/chineseocr/chineseocr.")

(defun org-ocr--base64-encode-image (filename)
  "Encode an image file as a base64 string."
  (base64-encode-string
   (with-temp-buffer
     (insert-file-contents filename)
     (buffer-string))
   t))

(defun org-ocr--find-link ()
  "Find the last image link before point."
  (org-link-unescape
   (save-excursion
     (let ((start (window-start)))
       (if (re-search-backward "\\[\\[\\(.*\\)\\]\\]" start t)
           ;; (buffer-substring-no-properties
           ;;  (match-beginning 1)
           ;;  (match-end 1))
           (org-element-property :path (org-element-context)))))))

(defun org-ocr--get-json-request-payload (image-string)
  "Generate the JSON format request payload for the image string."
  (format "{\"imgString\":\"data:image/png;base64,%s\"}" image-string))

(defun org-ocr--render-result (buffer result)
  "Render recognition result in a quote block."
  (with-current-buffer buffer
    (insert "\n#+BEGIN_QUOTE\n")
    (mapcar (lambda (res)
              (insert (format "%s\n" (assoc-default 'text res)))
              (message "%s" (assoc-default 'text res)))
            result)
    (insert "#+END_QUOTE"))
  (set-buffer-multibyte nil)
  (set-buffer-multibyte t))

(defun org-ocr--query-ocr (image-string)
  (let ((buffer (current-buffer)))
    (request
     org-ocr-api-url
     :type "POST"
     :data (org-ocr--get-json-request-payload image-string)
     :parser 'json-read
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (org-ocr--render-result buffer (assoc-default 'res data))))
     :error
     (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                    (message "Got error: %S" error-thrown))))))

(defun org-ocr-to-quote-block ()
  "Recognize the text on the last image before point and insert the
recognition result as a quote block at point."
  (interactive)
  (let ((link (org-ocr--find-link)))
    (message "sending %s..." link)
    (when link
      (org-ocr--query-ocr (org-ocr--base64-encode-image link)))))

(provide 'org-ocr)

;;; org-ocr.el ends here
