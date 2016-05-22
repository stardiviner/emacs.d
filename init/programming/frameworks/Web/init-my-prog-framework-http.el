;;; init-my-prog-framework-http.el --- init for HTTP
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(unless (boundp 'HTTP-prefix)
  (define-prefix-command 'HTTP-prefix))
(define-key my-prog-inferior-map (kbd "H") 'HTTP-prefix)


;;; [ restclient ]

(use-package restclient
  :ensure t
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

  ;; indent JSON in restclient REPL.
  (add-hook 'restclient-mode-hook
            (lambda ()
              (require 'js)
              (setq-local indent-line-function 'js-indent-line)))
  )


;;; [ company-restclient ]

(use-package company-restclient
  :ensure t
  :config
  (add-hook 'restclient-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'company-restclient)
              ))
  )


;;; [ ob-restclient ]

(use-package ob-restclient
  :ensure t)


;;; [ restclient-test ] -- Run tests with restclient.el

(use-package restclient-test
  :ensure t)


;; [ ob-http ] -- http request in org-mode babel

(use-package ob-http
  :ensure t)


;;; [ httprepl ]

(use-package httprepl
  :ensure t
  :config
  (define-key HTTP-prefix (kbd "H") 'httprepl)

  (add-hook 'httprepl-mode-hook
            (lambda ()
              (my-company-add-backend-locally 'company-restclient)
              ))
  )


;;; [ know-your-http-well ]

(use-package know-your-http-well
  :ensure t)


;;; [ httpcode ] -- explains the meaning of an HTTP status code.

(use-package httpcode
  :ensure t)


(provide 'init-my-prog-framework-http)

;;; init-my-prog-framework-http.el ends here
