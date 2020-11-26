;;; init-org-protocol.el --- init for Org Protocol
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ org-protocol ] -- intercept calls from emacsclient to trigger custom actions.

(use-package org-protocol
  :config
  (with-eval-after-load 'org-protocol
    (unless (featurep 'server)
      (require 'server))
    (unless (server-running-p)
      (server-start)))

  ;; [ org-protocol capture ]
  (setq org-capture-templates
        (append org-capture-templates
                `(("P" ,(format "%s\torg-protocol"
                                (all-the-icons-faicon "external-link" :face 'all-the-icons-orange)))
                  ("PP" ,(format "%s\tProtocol"
                                 (all-the-icons-faicon "chrome" :face 'all-the-icons-orange))
                   entry (file ,(concat org-directory "/Tasks/Tasks.org"))
                   "* %^{Title}\nSource: %u, %c\n#+begin_quote\n%i\n#+end_quote\n\n\n%?"
                   :prepend t
                   :empty-lines 1)
                  ("PL" ,(format "%s\tSave Link to Bookmarks"
                                 (all-the-icons-faicon "external-link" :face 'all-the-icons-orange))
                   entry (file ,(concat org-directory "/Bookmarks/Bookmarks.org"))
                   "* [[%:link][%:description]]\n:PROPERTIES:\n:DATE: %U \n:END:\n %?"
                   :prepend t
                   :empty-lines 1
                   :jump-to-captured t))))

  
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
  )

(use-package org-protocol-capture-html
  :ensure t
  :config
  (add-to-list 'org-capture-templates
               `("PH" ,(format "%s\torg-protocol-capture-html"
                               (all-the-icons-faicon "html5" :face 'all-the-icons-pink))
                 entry (file ,(concat org-directory "/Bookmarks/Bookmarks.org"))
                 "* [[%:link][%:description]]\n:PROPERTIES:\n:DATE: %U \n:END:\n %? \n%:initial"
                 :prepend t
                 :empty-lines 1
                 :jump-to-captured t)
               :append))


(provide 'init-org-protocol)

;;; init-org-protocol.el ends here
