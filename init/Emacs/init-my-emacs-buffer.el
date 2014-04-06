;;; init-my-emacs-buffer.el --- init Emacs buffer settings.

;;; Commentary:



;;; Code:


;;; [uniquify] -- meaningful names for buffers with the same name

(require 'uniquify)

(after 'uniquify
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*" ; don't muck with special buffers
        ))

(provide 'init-my-emacs-buffer)
