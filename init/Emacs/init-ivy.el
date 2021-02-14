;;; init-ivy.el --- init for Ivy-mode
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ ivy ]

(use-package ivy
  :ensure t
  :ensure ivy-hydra ; [C-o], [M-o]
  :delight ivy-mode
  :defer t
  :custom (ivy-initial-inputs-alist nil)
  :init (setq ivy-use-virtual-buffers t ; treat recentf, bookmarks as virtual buffers.
              ivy-virtual-abbreviate 'full ; use 'full to fix bookmarks uniquify issue.
              ivy-fixed-height-minibuffer t
              ivy-height 7)
  :hook (after-init . ivy-mode))

;;; [ ivy-rich ] -- More friendly display transformer for ivy.

;; (use-package ivy-rich
;;   :ensure t
;;   :defer t
;;   :commands (ivy-rich-mode)
;;   :hook (ivy-mdoe . ivy-rich-mode)
;;   :init
;;   ;;; [ all-the-icons-ivy-rich ] -- Better experience with icons for Ivy.
;;   (use-package all-the-icons-ivy-rich
;;     :ensure t
;;     :init (all-the-icons-ivy-rich-mode 1)
;;     ;; fix extra spaces between icon and text issue
;;     :config (setq all-the-icons-ivy-rich-icon-size 0.9)))

;;; [ counsel ] -- various completion functions using Ivy.

(use-package counsel
  :ensure t
  :defer t
  :delight counsel-mode
  :bind (([remap yank-pop] . counsel-yank-pop)
         ([remap menu-bar-open] . counsel-tmm) ; [F10] text menu access
         ([remap apropos] . counsel-apropos)
         ("C-x c p" . counsel-list-processes) ; [C-x c p]
         ("C-x c t" . cancel-function-timers) ; [C-x c t]
         ([remap list-colors-display] . counsel-colors-emacs)
         ("C-x c c" . counsel-colors-emacs)
         ("C-x c C" . counsel-colors-web)
         ("C-x RET v" . counsel-set-variable) ; [C-x RET v]
         ("C-x RET u" . counsel-unicode-char) ; [C-x RET u]
         ;; ([remap switch-to-buffer] . ivy-switch-buffer) ; [C-x b]
         ;; ([remap switch-to-buffer] . counsel-buffer-or-recentf) ; [C-x b]
         ("M-t" . counsel-git) ; [M-t]
         ("C-c v g g" . counsel-git-grep)
         ([remap grep] . counsel-grep) ; [C-s g]
         ;; ("" . counsel-switch-to-shell-buffer) ; switch to a shell buffer, or create one
         ([remap org-goto] . counsel-org-goto) ; [C-c C-j] completion for Org headings
         ;; ( . counsel-org-goto-all) ; completion for Org headings in all open buffers
         ;; ([remap org-set-tags-command] . counsel-org-tag) ; [C-c C-q]
         ;; ([remap org-agenda-set-tags] . counsel-org-tag-agenda) ; [:]
         ;; ([remap org-capture] . counsel-org-capture)
         ([remap org-attach-open] . counsel-org-file) ; browse all attachments for the current Org file
         ([remap locate] . counsel-locate)
         ("C-x c #" . counsel-linux-app)
         ;; :map read-expression-map ("C-r" . counsel-minibuffer-history) ; in [M-:]
         ;; :map ivy-minibuffer-map ("M-y" . ivy-next-line)
         )
  :preface (unless (boundp 'search-prefix) (define-prefix-command 'search-prefix))
  (define-key search-prefix (kbd "M-g") 'counsel-grep)
  (define-key search-prefix (kbd "M-r") 'counsel-rg) ; [C-u] prompt for dir support
  :init (setq counsel-mode-override-describe-bindings t)
  :hook (ivy-mode . counsel-mode))

;;; [ ivy-posframe ] -- Using posframe to show Ivy.

;; (use-package ivy-posframe
;;   :ensure t
;;   :after ivy
;;   :custom (;; (ivy-posframe-style 'window-bottom-left)
;;            (ivy-posframe-display-functions-alist
;;             '((swiper          . ivy-posframe-display-at-point)
;;               (complete-symbol . ivy-posframe-display-at-point)
;;               (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
;;               (t               . ivy-posframe-display-at-window-bottom-left)
;;               ;; (t               . ivy-posframe-display)
;;               )))
;;   :hook (after-init . ivy-posframe-mode)
;;   :config
;;   ;; fix ivy-posframe blink issue https://github.com/tumashu/ivy-posframe/issues/21
;;   (defvar ivy-posframe--first-show t)
;;   (defun ivy-posframe-cleanup ()
;;     "Cleanup ivy's posframe."
;;     (setq ivy-posframe--first-show t)
;;     (when (posframe-workable-p)
;;       (posframe-hide ivy-posframe-buffer)))
;;   (defun ivy-posframe--display (str &optional poshandler)
;;     "Show STR in ivy's posframe with POSHANDLER."
;;     (if (not (posframe-workable-p))
;;         (ivy-display-function-fallback str)
;;       (with-ivy-window
;;         (if (not ivy-posframe--first-show)
;;             (with-current-buffer ivy-posframe-buffer
;;               (erase-buffer)
;;               (insert str))
;;           (setq ivy-posframe--first-show nil)
;;           (apply #'posframe-show
;;                  ivy-posframe-buffer
;;                  :font ivy-posframe-font
;;                  :string str
;;                  :position (point)
;;                  :poshandler poshandler
;;                  :background-color (face-attribute 'ivy-posframe :background nil t)
;;                  :foreground-color (face-attribute 'ivy-posframe :foreground nil t)
;;                  :internal-border-width ivy-posframe-border-width
;;                  :internal-border-color (face-attribute 'ivy-posframe-border :background nil t)
;;                  :override-parameters ivy-posframe-parameters
;;                  (funcall ivy-posframe-size-function)))
;;         (ivy-posframe--add-prompt 'ignore)))
;;     (with-current-buffer ivy-posframe-buffer
;;       (setq-local truncate-lines ivy-truncate-lines)))
;;   )

;;; Ivy supports Chinese candidates filtering through Pinyin.
;;; https://emacs-china.org/t/topic/6069/22
(use-package pyim
  :ensure t
  :after ivy
  :config
  (defun pyim--ivy-cregexp (str)
    (let ((regex-sequence (ivy--regex-plus str)) ;; (ivy--regex-ignore-order str)
          (case-fold-search nil))
      (if (listp regex-sequence)
          (mapcar (lambda (regex)
                    (if (cdr regex)
                        (list (if (equal (car regex) "")
                                  ""
                                (pyim-cregexp-build (car regex)))
                              (cdr regex))
                      (list (pyim-cregexp-build (car regex)))))
                  regex-sequence)
        (if (string= regex-sequence "")
            regex-sequence
          (pyim-cregexp-build regex-sequence)))))
  (setq ivy-re-builders-alist '((read-file-name-internal . pyim--ivy-cregexp)
                                (kill-buffer . pyim--ivy-cregexp)
                                (switch-to-buffer . pyim--ivy-cregexp)
                                (ivy-switch-buffer . pyim--ivy-cregexp)
                                (org-refile . pyim--ivy-cregexp)
                                (org-set-tags-command . pyim--ivy-cregexp)
                                (org-insert-link . pyim--ivy-cregexp)
                                (ivy-read . pyim--ivy-cregexp)
                                (t . ivy--regex-plus))))


(provide 'init-ivy)

;;; init-ivy.el ends here
