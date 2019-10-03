;;; init-cscope.el --- init for cscope etc
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(unless (boundp 'cscope-prefix)
  (define-prefix-command 'cscope-prefix))
(define-key tags-prefix (kbd "c") 'cscope-prefix)


(defun cscope-build (dir)
  "My custom function to execute shell command: $ cscope -bR under `DIR'."
  (interactive "Dcscope build database directory: ")
  (let* ((dir (expand-file-name dir))
         (buffer-name (format "*cscope-build-%s" dir))
         (cscope-buffer (get-buffer-create buffer-name))
         process)
    (with-current-buffer cscope-buffer
      (if (get-buffer-process buffer-name)
          (kill-process (get-buffer-process buffer-name)))
      (setq default-directory dir)
      (setq process (start-file-process buffer-name buffer-name "cscope" "-bR"))
      (set-process-query-on-exit-flag process nil)
      (accept-process-output process 3)
      (if (looking-at "TODO: REGEXP about cscope build error")
          (progn
            (when cscope-buffer (kill-buffer cscope-buffer))
            (message "cscope build database failed"))
        (progn
          (message "cscope: database build %s : OK" dir))))
    cscope-buffer))

(define-key cscope-prefix (kbd "b") 'cscope-build)

;;; [ cscope ] -- An interface to Joe Steffen's "cscope" C browser.

;; Usage:
;;
;; - $ cscope -bR :: normal usage command.
;; - $ cscope -b -R -q -k ::
;; - `cscope-minor-mode' :: enable minor mode will enable keybindings.

;;; [ xcscope ] -- interface of cscope.

(use-package xcscope
  :ensure t
  :init (cscope-setup))

;;; [ helm-cscope ] -- Helm interface for xcscope.el.

(use-package helm-cscope
  :ensure t
  :init (add-hook 'c-mode-common-hook 'helm-cscope-mode)
  :config (add-hook 'helm-cscope-mode-hook
                    (lambda ()
                      (local-set-key (kbd "M-.") 'helm-cscope-find-global-definition)
                      (local-set-key (kbd "M-@") 'helm-cscope-find-calling-this-function)
                      (local-set-key (kbd "M-s") 'helm-cscope-find-this-symbol)
                      (local-set-key (kbd "M-,") 'helm-cscope-pop-mark))))


(cond
 ((featurep 'rscope)
  (define-key cscope-prefix (kbd "s") 'rscope-find-this-symbol)
  (define-key cscope-prefix (kbd "=") 'rscope-all-symbol-assignments)
  (define-key cscope-prefix (kbd "d") 'rscope-find-global-definition)
  (define-key cscope-prefix (kbd "c") 'rscope-find-functions-calling-this-function)
  (define-key cscope-prefix (kbd "C") 'rscope-find-called-functions)
  (define-key cscope-prefix (kbd "t") 'rscope-find-this-text-string)
  (define-key cscope-prefix (kbd "i") 'rscope-find-files-including-file)
  (define-key cscope-prefix (kbd "h") 'rscope-find-calling-hierarchy))

 ;; [ cscope ]
 (t (define-key cscope-prefix (kbd "c") 'cscope-find-this-symbol)))

(define-key cscope-prefix (kbd "n") 'cscope-history-backward-line-current-result)
(define-key cscope-prefix (kbd "N") 'cscope-history-forward-file-current-result)


(provide 'init-cscope)

;;; init-cscope.el ends here
