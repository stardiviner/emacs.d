;;; init-my-tool-bbdb.el --- init BBDB
;;; -*- coding: utf-8 -*-

;;; Commentary:


;;; Help:
;; http://savannah.nongnu.org/projects/bbdb/
;; http://www.mail-archive.com/bbdb-info@lists.sourceforge.net/
;; http://www.emacswiki.org/cgi-bin/wiki/CategoryBbdb


;;; Code:

;;; [ BBDB ]

;;; Usage:
;; - [M-x bbdb-help]
;; - [M-x bbdb-info]
;; - [M-x bbdb] :: start BBDB
;; - [M-x bbdb-merge-file] :: merge databases
;;        If you want to merge two very different databases (one from your girlfriend, one from yourself), your best bet is to just concatenate the two. Then start the BBDB, and use M-x bbdb-show-duplicates. For every record that appears twice, put point on the second record, and use ‘r’ to merge it into the first record.
;; - Export
;;   http://www.emacswiki.org/emacs/BbdbExporters
;; - Import
;;   http://www.emacswiki.org/emacs/BbdbImporters

;;; TODO: BBDBv3

;; (require 'bbdb)



;;; [ bbdb-vcard ]

;; (require 'bbdb-vcard)


;;; helm-bbdb.el




(provide 'init-my-tool-bbdb)

;;; init-my-tool-bbdb.el ends here
