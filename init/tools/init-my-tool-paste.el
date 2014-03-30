;;; init-my-tool-paste.el --- init Emacs paste tool

;;; Commentary:

;;; Code:

;;; [ gist.el ]
;; Usage:
;;   - Functions:
;;      - gits-list -- lists your gists in a new buffer, use RET to open one in other buffer
;;      - gist-region -- copies Gist URL into the kill ring.
;;                      with a prefix argument, makes a private gist.
;;      - gist-region-private -- explicitly create a private gist.
;;      - gist-buffer -- copies Gist URL into the kill ring.
;;                       with a prefix argument, makes a private gist.
;;      - gist-buffer-private - explicitly create a private gist.
;;      - gits-region-or-buffer
;;      - gist-region-or-buffer-private


(require 'gist)

(setq gist-view-gist t) ; view your Gist using `browse-url` after it is created.


(provide 'init-my-tool-paste)

;;; init-my-tool-paste.el ends here
