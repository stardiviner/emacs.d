;;; init-cscope.el --- init for cscope etc
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(defun cscope-build-tags-database (dir)
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

(define-key tags-prefix (kbd "b") 'cscope-build-tags-database)

;;; [ cscope ] -- An interface to Joe Steffen's "cscope" C browser.

;; Usage:
;;
;; - $ cscope -bR :: normal usage command.
;; - $ cscope -b -R -q -k ::
;; - `cscope-minor-mode' :: enable minor mode will enable keybindings.

;;; [ xcscope ] -- interface of cscope.

(use-package xcscope
  :ensure t
  :hook ((c-mode . cscope-minor-mode)
         (c++-mode . cscope-minor-mode))
  ;; :preface
  ;; re-define cscope-minor-mode keymap
  ;; (define-key cscope-minor-mode-keymap cscope-keymap-prefix nil)
  ;; (setq cscope-keymap-prefix (kbd "M-g t"))
  ;; (define-key cscope-minor-mode-keymap cscope-keymap-prefix cscope-command-map)
  :bind (:map c-mode-map
              ("M-." . cscope-find-this-symbol)
              ("M-s" . cscope-find-global-definition)
              ("M-@" . cscope-find-calling-this-function)
              ("M-," . cscope-pop-mark))
  :config
  (define-key cscope-list-entry-keymap (kbd "C-n") 'cscope-history-forward-file)
  (define-key cscope-list-entry-keymap (kbd "C-p") 'cscope-history-backward-file)
  (define-key cscope-list-entry-keymap (kbd "n") 'cscope-history-forward-line-current-result)
  (define-key cscope-list-entry-keymap (kbd "p") 'cscope-history-backward-line-current-result))


(provide 'init-cscope)

;;; init-cscope.el ends here
