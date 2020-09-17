;;; init-CouchDB.el --- init for CouchDB.

;;; Time-stamp: <2018-08-11 13:05:40 stardiviner>

;;; Commentary:



;;; Code:

;;; [ libelcouch ] -- Elisp library to communication with CouchDB.

(use-package libelcouch
  :ensure t)

;;; [ elcouch ] -- View and manipulate CouchDB databases.

(use-package elcouch
  :ensure t
  :commands (elcouch-open))



(provide 'init-CouchDB)

;;; init-CouchDB.el ends here
