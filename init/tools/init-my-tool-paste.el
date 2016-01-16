;;; init-my-tool-paste.el --- init Emacs paste tool

;;; Commentary:

;;; Code:


(unless (boundp 'paste-prefix)
  (define-prefix-command 'paste-prefix))
(define-key my-tools-prefix (kbd "p") 'paste-prefix)


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


;; (require 'gist)
;;
;; (setq gist-view-gist t) ; view your Gist using `browse-url` after it is created.
;;
;; ;;; TODO: enable gist-mode in Org-mode.
;; ;; FIXME: gist-mode enable will lead to Org-mode [C-x C-s] error.
;;
;; ;;; --------------------------------------------------
;; (define-key paste-prefix (kbd "p") 'gist-region-or-buffer)
;; (define-key paste-prefix (kbd "v") 'gist-region-or-buffer-private)
;; (define-key paste-prefix (kbd "r") 'gist-region)
;; (define-key paste-prefix (kbd "b") 'gist-buffer)
;; (define-key paste-prefix (kbd "l") 'gist-list)
;; ;;; --------------------------------------------------


;;; [ yagist ] -- Yet Another gist

;;; Usage:
;;
;; - `yagist-list' :: list your gists in a new buffer.
;; - `yagist-region' :: Copies Gist URL into the kill ring.
;; - `yagist-region-private' :: Explicitly create a private gist.
;; - `yagist-buffer' :: Copies Gist URL into the kill ring.
;; - `yagist-buffer-private' :: Explicitly create a private gist.
;; - `yagist-region-or-buffer' :: Post either the current region, or if mark is not set,
;;                                the current buffer as a new paste at gist.github.com .
;;                                Copies the URL into the kill ring.
;;                                With a prefix argument, makes a private paste.
;; - `yagist-region-or-buffer-private' :: Explicitly create a gist from the region or buffer.
;; - `yagist-minor-mode' :: Automated POST current buffer contents to gist after saving.
;; - `yagist-global-minor-mode' :: Open the file that under gist repository automatically activate `yagist-minor-mode'.

(require 'yagist)

;; *Encrypt your token by using `kaesar' package with AES encryption algorithm.
;; (setq yagist-encrypt-risky-config t)

(setq yagist-view-gist t ; view gist URL after posted.
      yagist-working-directory "~/.gist"
      ;; yagist-working-directory-alist
      yagist-github-token "03fdc1142f1eaa170ee1508284513c9ce89cadb0"
      )

(define-key paste-prefix (kbd "p") 'yagist-region-or-buffer)
(define-key paste-prefix (kbd "P") 'yagist-region-or-buffer-private)
(define-key paste-prefix (kbd "r") 'yagist-region)
(define-key paste-prefix (kbd "b") 'yagist-buffer)
(define-key paste-prefix (kbd "l") 'yagist-list)



;;; htmlize-buffer (convert current buffer into HTML output)
(define-key paste-prefix (kbd "h") 'htmlize-buffer)



(provide 'init-my-tool-paste)

;;; init-my-tool-paste.el ends here
