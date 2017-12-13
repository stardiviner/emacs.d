;;; init-my-org-protocol.el --- init for Org Protocol
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ org-protocol ] -- intercept calls from emacsclient to trigger custom actions.

(require 'server)
(unless (server-running-p)
  (server-start))
(require 'org-protocol)

;; ;; TODO: setup this option.
;; ;; (module-name :property value property: value ...)
;; (setq org-protocol-project-alist
;;       '(("http://orgmode.org/worg/"
;;          :online-suffix ".php"
;;          :working-suffix ".org"
;;          :base-url "http://orgmode.org/worg/"
;;          :working-directory "/home/stardiviner/Org/Worg/")
;;         ("http://localhost/org-notes/"
;;          :online-suffix ".html"
;;          :working-suffix ".org"
;;          :base-url "http://localhost/org/"
;;          :working-directory "/home/user/org/"
;;          :rewrites (("org/?$" . "index.php")))))


(provide 'init-my-org-protocol)

;;; init-my-org-protocol.el ends here
