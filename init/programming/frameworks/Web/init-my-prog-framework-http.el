;;; init-my-prog-framework-http.el --- init for HTTP
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'HTTP-prefix)
  (define-prefix-command 'HTTP-prefix))
(define-key my-prog-inferior-map (kbd "H") 'HTTP-prefix)


;;;_ restclient

;;; This is a tool to manually explore and test HTTP REST webservices. Runs
;;; queries from a plain-text query sheet, displays results as a pretty-printed
;;; XML, JSON and even images.

;;; Usage:
;;
;; `restclient-mode' is a major mode which does a bit of highlighting and supports
;; a few additional keypresses:
;;
;; buffer example:
;;
;;   GET http://www.example.com
;;   # use comment as separator for queries.
;;   :username = chris
;;   :password := (read (file "filename.txt"))
;;   POST http://www.example.com/?action=login&:username&:password
;;
;; for localhost
;; GET http://127.0.0.1:3000
;;
;; - [C-c C-c] :: runs the query under the cursor, tries to pretty-print the response (if possible)
;; - [C-c C-r] :: same, but doesn't do anything with the response, just shows the buffer
;; - [C-c C-v] :: same as C-c C-c, but doesn't switch focus to other window
;; - [C-c C-p] :: jump to the previous query
;; - [C-c C-n] :: jump to the next query
;; - [C-c C-.] :: mark the query under the cursor
;; - [C-c C-u] :: `restclient-copy-curl-command' :: copy curl command format to clipboard.
;;
;; `restclient-http-do-hook'

(use-package restclient
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

  (setq restclient-log-request t
        restclient-same-buffer-response t
        ;; restclient-same-buffer-response-name "*HTTP Response*"
        restclient-inhibit-cookies nil)
  
  (defun restclient-add-separator ()
    (interactive)
    (insert "# separator\n\n"))
  
  (define-key restclient-mode-map (kbd "C-c C-'") 'restclient-add-separator)
  
  (defun restclient-new-buffer ()
    (interactive)
    (let ((buffer (generate-new-buffer "*rest-client*")))
      (with-current-buffer buffer
        (insert "# -*- restclient -*- \n\n")
        (restclient-mode)
        (pop-to-buffer buffer))))

  (define-key HTTP-prefix (kbd "r") 'restclient-new-buffer)

  ;; Org-mode Babel integration
  ;; TODO: (load "~/.emacs.d/init/extensions/ob-rest.el")

  ;; TEST: indent json in restclient-mode
  (add-hook 'restclient-mode-hook
            (lambda ()
              (require 'js)
              (setq-local indent-line-function 'js-indent-line)))
  )


;;;_ know-your-http-well

(require 'know-your-http-well)

;;;_ company-restclient

;; Features
;;
;; - HTTP method name completion
;; - HTTP header name completion
;; - If header name starts with uppercase character, the completion result is capitalized (e.g. "Content-Type").
;; - Otherwise, the completion result contains lowercase characters only (e.g. "content-type").
;; - Description about HTTP method and header is shown in minibuffer
;; - Variable name completion

(add-hook 'restclient-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-restclient)))


;;;_ httprepl

(use-package httprepl
  :config
  (define-key HTTP-prefix (kbd "H") 'httprepl)
  )


;;; [ httpcode ] -- explains the meaning of an HTTP status code

;;; Usage:
;;
;; - [M-x hc]


;;;_ web -- useful HTTP client



(provide 'init-my-prog-framework-http)

;;; init-my-prog-framework-http.el ends here
