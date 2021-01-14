;;; init-HTTP-RESTful.el --- init for HTTP RESTful

;;; Commentary:



;;; Code:

;;; [ restclient ]

(use-package restclient
  :ensure t
  :defer t
  :mode ("\\.http\\'" . restclient-mode)
  :init
  (setq restclient-log-request t
        restclient-same-buffer-response t
        ;; restclient-same-buffer-response-name "*HTTP Response*"
        restclient-inhibit-cookies nil)
  :config
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

  ;; indent JSON in restclient REPL.
  (add-hook 'restclient-mode-hook
            (lambda ()
              (require 'js)
              (setq-local indent-line-function 'js-indent-line)))

  ;; Edit restclient JSON body in a narrow buffer.
  (defun my-restclient-indirect-edit ()
    "Use `edit-indirect-region' to edit the request body in a separate buffer."
    (interactive)
    (save-excursion
      (goto-char (restclient-current-min))
      (when (re-search-forward restclient-method-url-regexp (point-max) t)
        (forward-line)
        (while (cond
                ((and (looking-at restclient-header-regexp) (not (looking-at restclient-empty-line-regexp))))
                ((looking-at restclient-use-var-regexp)))
          (forward-line))
        (when (looking-at restclient-empty-line-regexp)
          (forward-line))
        (edit-indirect-region (min (point) (restclient-current-max)) (restclient-current-max) t))))

  (define-key restclient-mode-map (kbd "C-c '") 'my-restclient-indirect-edit)

  ;; [ company-restclient ]
  (use-package company-restclient
    :ensure t
    :init
    (defun my/company-restclient-setup ()
      (my-company-add-backend-locally 'company-restclient))
    (add-hook 'restclient-mode-hook #'my/company-restclient-setup))

  ;; [ restclient-test ] -- Run tests with restclient.el
  (use-package restclient-test
    :ensure t))


;; [ ob-restclient ]

(use-package ob-restclient
  :ensure t
  :defer t
  :commands (org-babel-execute:restclient)
  :config
  (add-to-list 'org-babel-load-languages '(restclient . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("restclient" . "http")))

;; [ ob-http ] -- http request in org-mode babel

(use-package ob-http
  :ensure t
  :defer t
  :commands (org-babel-execute:http)
  :config
  (add-to-list 'org-babel-load-languages '(http . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("http" . "http")))

;;; [ httprepl ]

(use-package httprepl
  :ensure t
  :defer t
  :commands (httprepl)
  :config (add-hook 'httprepl-mode-hook #'my/company-restclient-setup nil 'local))

;;; [ know-your-http-well ]

(use-package know-your-http-well
  :ensure t
  :defer t
  :after company-restclient)

;;; [ httpcode ] -- explains the meaning of an HTTP status code.

(use-package httpcode
  :ensure t
  :defer t
  :bind (:map restclient-mode-map ("C-c C-d" . hc)
              :map ob-http-mode-map ("C-c C-d" . hc)))

;;; [ walkman ] -- Write HTTP requests in Org mode and replay them at will using cURL.

(use-package walkman ; [C-c C-'] in Org Mode
  :ensure t
  :defer t
  :init (add-to-list 'display-buffer-alist '("^\\*walkman\\*" . (display-buffer-below-selected)))
  (walkman-setup))



(provide 'init-HTTP-RESTful)

;;; init-HTTP-RESTful.el ends here
