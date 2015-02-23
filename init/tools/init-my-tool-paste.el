;;; init-my-tool-paste.el --- init Emacs paste tool

;;; Commentary:

;;; Code:


(unless (boundp 'paste-map)
  (define-prefix-command 'paste-map))

(define-key my-tools-prefix-map (kbd "p") 'paste-map)


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

;;; TODO: enable gist-mode in Org-mode.
;; FIXME: gist-mode enable will lead to Org-mode [C-x C-s] error.

;;; --------------------------------------------------

(define-key paste-map (kbd "p") 'gist-region-or-buffer)
(define-key paste-map (kbd "v") 'gist-region-or-buffer-private)
(define-key paste-map (kbd "r") 'gist-region)
(define-key paste-map (kbd "b") 'gist-buffer)
(define-key paste-map (kbd "l") 'gist-list)
;;; --------------------------------------------------



;;; htmlize-buffer (convert current buffer into HTML output)
(define-key paste-map (kbd "h") 'htmlize-buffer)




(provide 'init-my-tool-paste)

;;; init-my-tool-paste.el ends here
