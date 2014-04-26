;;; init-my-prog-document.el ---

;;; Commentary:

;;; Code:

;;; keybindings prefix

(unless (boundp 'my-help-document-prefix-map)
  (define-prefix-command 'my-help-document-prefix-map))

;; FIXME: don't set prefix as global, set it major mode locally.
;; (global-set-key (kbd "C-h d") 'my-help-document-prefix-map)
;; (define-key 'major-mode (kbd "C-h d") 'my-help-document-prefix-map)

;;; e.g.
;;; with my-help-document-prefix-map prefix.
;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-h d") 'my-help-document-prefix-map)
;;             (define-key my-help-document-prefix-map (kbd "d") 'yari)
;;             (define-key my-help-document-prefix-map (kbd "D") 'yari-helm)
;;             ))



;;; [ ElDoc ] --- show you the argument list of the function call you are currently writing in the echo area.

(require 'eldoc)

(dolist (hook
         '(emacs-lisp-mode-hook
           lisp-interaction-mode-hook
           lisp-mode-hook
           ielm-mode-hook))
  (add-hook hook 'turn-on-eldoc-mode))


(set-face-attribute 'eldoc-highlight-function-argument nil
                    :underline t :foreground "cyan"
                    :weight 'bold)


;;; ElDoc with most paredit command.
;;; whenever the listed commands are used, ElDoc will automatically refresh the minibuffer.
(eldoc-add-command 'paredit-backward-delete
                   'paredit-close-round)

;;; add docstring to ElDoc output.
;;; TODO
;; (defadvice eldoc-get-fnsym-args-string (after add-dacstring (sym)
;;                                               activate compile)
;;   "Add a doc string to ElDoc's modeline information."
;;   (let ((doc (eldoc-docstring-first-line
;;               (cdr (help-split-fundoc (documentation sym t) sym)))))
;;     (when (and doc (not (equal doc "")))
;;       (setq ad-return-value
;;             (concat ad-return-value
;;                     (if (> (+ (length ad-return-value) (length doc) 4)
;;                            (frame-width)) "\n" "    ")
;;                     doc))))
;;   ad-return-value)


;;; [ which-function-mode (which-func) ]

(require 'which-func)

;; (setq which-func-modes t)
(add-to-list 'which-func-modes 'org-mode)

(which-function-mode 1)



(provide 'init-my-prog-document)

;;; init-my-prog-document.el ends here
