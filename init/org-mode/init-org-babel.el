;;; init-org-babel.el --- init for Org Babel
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq org-confirm-babel-evaluate nil)
(setq org-babel-hash-show-time t) ; header argument: :cache yes.
(setq org-src-tab-acts-natively nil)

(add-to-list 'display-buffer-alist
             '("^\\*Org-Babel Results\\*" .
               (display-buffer-reuse-window display-buffer-below-selected)))

;;; add org-babel header-args property into default properties list.
(add-to-list 'org-default-properties "header-args")

;;; source block default header arguments
(setq-default org-babel-default-header-args
              '((:session . "none")
                (:noweb . "no") (:hlines . "no")
                (:tangle . "no") ; (:comments . "link")
                (:cache . "yes")
                (:results . "replace")
                (:mkdirp . "yes") ; auto create specified path directory
                ;; for exporting
                (:eval . "never-export")
                ;; (:exports . "both") conflict with (:eval "never-export")
                ))

;;; don't evaluate babel when exporting. For helper functions like
;;; `my:org-convert-region-to-html' etc.
(setq org-export-use-babel nil)


;; babel src block editing
(setq org-src-window-setup `,(if (< (frame-width) 200)
                                 'current-window
                               'split-window-right)
      ;; 0: fix `diff' babel syntax highlighting invalid issue.
      org-edit-src-content-indentation 0)

;;; Manage org-babel popup buffers with `display-buffer-alist'.
(add-to-list 'display-buffer-alist
             '("^\\*Org- Src.*\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Org-Babel Preview.*\\*" (display-buffer-below-selected)))
(add-to-list 'display-buffer-alist
             '("^\\*Org-Babel Error Output\\*" (display-buffer-below-selected)))

;;; [ inline source block: src_{lang}[switchs,flags]{body} ]

;; (setq org-babel-exp-inline-code-template "src_%lang[%switches%flags]{%body}")
(setq org-babel-default-inline-header-args
      '((:session . "none")
        (:results . "replace")
        ;; (:exports . "both")
        (:hlines . "yes")
        (:eval . "never-export")))

;;; [ noweb ]
;; Raise errors when noweb references don't resolve.
(setq org-babel-noweb-error-all-langs t)

;;; default loading babel language.
(setq org-babel-load-languages '((org . t)))

;;; [ ob-shell ]

(use-package ob-shell
  :defer t
  :commands (org-babel-execute:shell org-babel-shell-initialize)
  :init (org-babel-shell-initialize)
  :config
  (add-to-list 'org-babel-load-languages '(shell . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("shell" . "sh"))
  (add-to-list 'org-babel-default-header-args:sh
               '(:results . "output"))
  (add-to-list 'org-babel-default-header-args:sh
               '(:noweb . "yes"))
  (add-to-list 'org-babel-default-header-args:shell
               '(:results . "output"))
  (add-to-list 'org-babel-default-header-args:shell
               '(:noweb . "yes")))

;;; [ Tangle ]

(add-to-list 'org-babel-tangle-lang-exts '("latex" . "tex"))
(add-to-list 'org-babel-tangle-lang-exts '("awk" . "awk"))

;;; [ org-tanglesync ] -- sync external changes back into an org-mode source block.


;;; [ Library of Babel ]

;;; automatically ingest "Library of Babel".
(with-eval-after-load 'org
  (org-babel-lob-ingest
   (concat user-emacs-directory "Org-mode/Library of Babel/Library of Babel.org")))

;;; [ Literate dotfiles management with Org-mode ]

;; Tangle Org files when we save them
;; (defun tangle-on-save-org-mode-file()
;;   (when (string= (message "%s" major-mode) "org-mode")
;;     (org-babel-tangle)))
;; (add-hook 'after-save-hook 'tangle-on-save-org-mode-file)

;;; [ coderef ]
;;; prepend comment char ahead of `org-coderef-label'.
;; auto prefix with comment char when create code ref in src block with `org-store-link'.
(defun org-src-coderef-format-prepend-comment (result)
  "Auto prefix with comment char before `org-coderef-label' `RESULT'."
  ;;; notice `org-src-coderef-format' is invoked twice. got two different `comment-start' "#" and ";".
  (if (string= org-coderef-label-format "(ref:%s)")
      (setq-local org-coderef-label-format
                  (format "%s %s" comment-start org-coderef-label-format))
    org-coderef-label-format))
;; FIXME: (advice-add 'org-src-coderef-format :filter-return 'org-src-coderef-format-prepend-comment)

;;; ding around source block execution.
(add-hook 'org-src-mode-hook #'sound-tick)
(add-hook 'org-babel-after-execute-hook #'sound-voice-complete)
(add-hook 'org-babel-post-tangle-hook #'sound-voice-complete)
(add-hook 'org-babel-pre-tangle-hook #'sound-tick)

;;; Tangling with append to file instead of default overwrite.
(defun my/org-babel-tangle-append ()
  "Append source code block at point to its tangle file.
The command works like `org-babel-tangle' with prefix arg
but `delete-file' is ignored."
  (interactive)
  (cl-letf (((symbol-function 'delete-file) #'ignore))
    (org-babel-tangle '(4))))
(define-key org-babel-map (kbd "M-t") 'my/org-babel-tangle-append)

;;; NOTE: This advice caused long suspend on babel source block suspend.
;;; auto re-display inline images when source block generate graphics image file link result.
;; (defun my/org-redisplay-inline-images (&optional arg info params)
;;   (org-redisplay-inline-images))
;; (advice-add 'org-babel-execute-src-block :after #'my/org-redisplay-inline-images)

;;; interactively insert noweb of completing named src blocks. [C-c C-v M-q]
(defun my/org-babel-insert-noweb-of-named-src-block (&optional template)
  (interactive)
  (let ((src-block
	       (completing-read "Enter src block name[or TAB or ENTER]: "
                          (org-babel-src-block-names))))
    (unless (string-equal "" src-block)
	    (insert (format "<<%s>>" src-block)))))
(define-key org-babel-map (kbd "M-q") 'my/org-babel-insert-noweb-of-named-src-block)

(defun my/org-babel-insert-call-of-named-src-block (&optional template)
  "Interactively insert a named src block call with `TEMPLATE'."
  (interactive)
  (let ((template (or template "#+call: %s()\n"))
	      (src-block (completing-read "Enter src block name[or TAB or ENTER]: "
                                    (org-babel-src-block-names))))
    (unless (string-equal "" src-block)
	    (insert (format template src-block)))))
(define-key org-babel-map (kbd "M-c") 'my/org-babel-insert-call-of-named-src-block)

;;; [ helm-lib-babel ] -- inserting a #+CALL: reference to an source block name.

;;; interactively insert #+CALL of completing named src blocks. [C-c C-v M-i]
(use-package helm-lib-babel
  :if (not (fboundp 'my/org-babel-insert-call-of-named-src-block))
  :ensure t
  :defer t
  :init (defalias 'my/org-babel-insert-helm-lib 'helm-lib-babel-insert)
  :bind (:map org-babel-map ("M-i" . my/org-babel-insert-helm-lib)))

;;; [ ob-async ] -- asynchronous execution of org-babel src blocks for *any* languages.

(use-package ob-async
  :ensure t
  :config
  ;; FIX: void variable `inferior-julia-program-name'.
  (add-hook 'ob-async-pre-execute-src-block-hook
            '(lambda () (setq inferior-julia-program-name "julia"))))

;;; [ org-babel-eval-in-repl ] -- eval org-babel block code with eval-in-repl.el

;; (use-package org-babel-eval-in-repl
;;   :ensure t
;;   :bind (:map org-mode-map
;;               ("C-<return>" . ober-eval-in-repl)
;;               ("C-c C-<return>" . ober-eval-block-in-repl))
;;   :config (with-eval-after-load "eval-in-repl"
;;             (setq eir-jump-after-eval nil)))

;;; [ org-radiobutton ] -- Get the checked item from a check list to be used for
;;; Org-mode Literate Programming variable.

;; (use-package org-radiobutton
;;   :ensure t
;;   :defer t
;;   :init (global-org-radiobutton-mode))

;; load all languages AT LAST.
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)



(provide 'init-org-babel)

;;; init-org-babel.el ends here
