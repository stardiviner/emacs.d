;;; init-my-prog-indent.el --- indent init for programming

;;; Commentary:


;;; Code:


(setq standard-indent 4)


;;; electric-indent-mode

;; (electric-indent-mode t)


;;; [ custom indent functions ]

(global-set-key (kbd "C-c >")
                (lambda (start end)
                  (interactive "rP")
                  (indent-rigidly-right-to-tab-stop start end)))

(global-set-key (kbd "C-c <")
                (lambda (start end)
                  (interactive "rP")
                  (indent-rigidly-left-to-tab-stop start end)))



;; (defun text-shift-region (start end count)
;;   "Indent lines from START to END by COUNT spaces."
;;   (save-excursion
;;     (goto-char end)
;;     (beginning-of-line)
;;     (setq end (point))
;;     (goto-char start)
;;     (beginning-of-line)
;;     (setq start (point))
;;     (indent-rigidly start end count)))
;;
;; (defun text-shift-region-right (start end &optional count)
;;   "Shift region of code to the right
;;    Stolen from python-mode.
;;    The lines from the line containing the start of the current region up
;;    to (but not including) the line containing the end of the region are
;;    shifted to the right, by `text-indent-offset' columns.
;;
;;    If a prefix argument is given, the region is instead shifted by that
;;    many columns.  With no active region, indent only the current line."
;;   (interactive
;;    (let ((p (point))
;;          (m (mark))
;;          (arg current-prefix-arg))
;;      (if m
;;          (list (min p m) (max p m) arg)
;;        (list p (save-excursion (forward-line 1) (point)) arg))))
;;   (text-shift-region start end (prefix-numeric-value
;;                                 (or count text-indent-offset)))
;;   )
;;
;; ;; Code in StackOverflow must be marked by four spaces at the
;; ;; beginning of the line
;; (setq text-indent-offset 4)
;; (global-set-key "\C-c>" 'text-shift-region-right)


;;; [ auto-indent-mode ]
;; (require 'auto-indent-mode)

;; (setq
;;  ;; auto-indent-indent-style 'moderate     ; 'aggressive, 'moderate, 'conservative.
;;  auto-indent-fix-org-return t ; allow newline and indent behavior in source code blocks in org-mode.
;;  auto-indent-fix-org-backspace t
;;  auto-indent-fix-org-auto-fill t
;;  auto-indent-fix-org-yank t ; allow org-mode yanks to be indented in source code blocks of org-mode.
;;  auto-indent-fix-org-move-beginning-of-line t
;;  ;; auto-indent-block-close
;;  ;; auto-indent-engine
;;  )

;; (auto-indent-global-mode 1)


;;; [ clean-aindent-mode ] -- clean auto-indent and backspace unindent

;; https://github.com/pmarinov/clean-aindent-mode



;; ;;; [ highlight-indentation ]

;; (require 'highlight-indentation)

;; (setq highlight-indentation-offset 4) ; default offset if have not found offset in major mode.
;; ;; (highlight-indentation-set-offset) ; set offset buffer locally.

;; (set-face-attribute 'highlight-indentation-face nil
;;                     :background "#00232C"
;;                     :inherit nil)
;; (set-face-attribute 'highlight-indentation-current-column-face nil
;;                     :background "slate blue"
;;                     :inherit nil)

;; (dolist (hook '(ruby-mode-hook
;;                 python-mode-hook
;;                 shell-mode-hook
;;                 html-mode-hook
;;                 css-mode-hook
;;                 c-mode-hook
;;                 c++-mode-hook
;;                 go-mode-hook
;;                 lua-mode-hook
;;                 php-mode-hook
;;                 ))
;;   (add-hook hook '(lambda ()
;;                     ;; displays guidelines indentation (space indentation only)
;;                     (highlight-indentation-mode t)
;;                     ;; displays guidelines for the current-point indentation (space indentation only)
;;                     (highlight-indentation-current-column-mode)
;;                     )))

;; TODO: ignore first 1, (or and 2) level indent guide line?


;;; [ indent-hints ] -- Get some hints about whether your buffer is space- or tab-loving
;; Usage:
;; 1.
;; Just check out your mode-line to see whether the buffer you're visiting is space-loving or tab-loving. It also shows the ratio of space-to-tab (or tab-to-space, whichever your buffer loves) loving-ness that your current buffer exudes. Here's a "screenshot":
;;            test.el Top -- (Emacs-Lisp \t:0.53 yas pair)--etc. etc.--
;; The file being visited in the "screenshot" has more tabs than spaces (53% of the lines that start with some sort of indentation start with tabs, to be exact).
;; 2.
;; You can also add your own custom functions to the hooks indent-hints-mode-tab-loving-hook and indent-hints-mode-space-loving-hook which run after a buffer is detected to be tab-loving or space-loving, respectively.
;; 3. Functions:
;; - (ih/activate-space-loving-profile) --> Activate the space-loving profile.
;; - (ih/activate-tab-loving-profile)   --> Activate the tab-loving profile.
;; - (indent-hints-global-activate)     --> Sets up the minor-mode-alist and buffer-local variable for indentation hints.
;; - (indent-hints-mode-on)             --> Turns on indent-hints-mode, if appropriate. This function is intended to be used with define-globalized-minor-mode.


;; ;; (require 'indent-hints)

;; (setq indent-hints-tab-loving-modeline-indicator " \\t")
;; ;; You can set up some "whitespace profiles" that get selected automatically
;; ;; when a buffer is detected to be tab-loving or space-loving.
;; (setq indent-hints-profile-switching-enabled t)

;; ;; (indent-hints-global-mode t)


;;; [ indent-hint.el ]
;; TODO (load "~/.emacs.d/my-init/extensions/indent-hint.el")
;; TODO 1. diff with indent/indent-hint.el file.
;;    (load "~/.emacs.d/extensions/indent/indent-hint-files/indent-hint-lisp.el")
;; TODO change indent line style and colors.

;; (dolist (hook '(;; emacs-lisp-mode-hook
;;                 lisp-mode-hook
;;                 ;; lisp-interaction-mode-hook
;;                 ))
;;   (add-hook hook 'indent-hint-lisp))


;;; [ indent-guide ]

(use-package indent-guide
  :config
  (setq indent-guide-delay nil
        indent-guide-recursive t
        ;; - 0 to avoid zero-column guide line.
        ;; - -1 to show all indent lines.
        indent-guide-threshold 0
        )

  ;; works with `indent-guide-global-mode'
  (add-to-list 'indent-guide-inhibit-modes 'org-mode)
  (add-to-list 'indent-guide-inhibit-modes 'web-mode)
  (add-to-list 'indent-guide-inhibit-modes 'emacs-lisp-mode)

  ;; custom indent line char
  ;; 1: use `indent-guide-char'.
  ;; : │ ┃ ▏┃ | ❘ │ ┃ ▍ ┇ ┋ ┊ ┆ ╽ ╿ ▏▕
  (setq indent-guide-char "╿")
  (set-face-attribute 'indent-guide-face nil
                      ;; :foreground "olive drab"
                      ;; :foreground "dark violet"
                      :foreground (color-lighten-name (face-background 'default) 6)
                      )

  ;; 2: use face-attribute stipple pixmap data.
  ;; (setq indent-guide-char " ")
  ;; (set-face-attribute 'indent-guide-face nil
  ;;                     :foreground "cyan"
  ;;                     :inherit nil
  ;;                     :stipple (list 7 4 (string 16 0 0 0)))

  ;; (indent-guide-global-mode)

  (dolist (hook '(prog-mode-hook
                  ;; emacs-lisp-mode-hook
                  ;; lisp-mode-hook
                  clojure-mode-hook
                  ruby-mode-hook
                  python-mode-hook
                  ))
    (add-hook hook
              (lambda ()
                (unless (member major-mode indent-guide-inhibit-modes)
                  (indent-guide-mode 1))
                )))

  ;; (indent-guide-post-command-hook)
  )


;;; [ aggressive-indent-mode ]

(use-package aggressive-indent
  :config
  ;; The variable `aggressive-indent-dont-indent-if' lets you customize when you
  ;; **don't** want indentation to happen.  For instance, if you think it's
  ;; annoying that lines jump around in `c++-mode' because you haven't typed the
  ;; `;' yet, you could add the following clause:
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c++-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line)))))

  ;; (global-aggressive-indent-mode)
  ;; or
  (dolist (hook '(prog-mode-hook
                  ))
    (add-hook hook #'aggressive-indent-mode))

  ;; FIXME: seems aggressive-indent-mode does not work well with ruby-mode & enh-ruby-mode.
  ;; ruby-mode can't auto complete "end".
  ;; (dolist (hook '(ruby-mode-hook
  ;;                 enh-ruby-mode-hook
  ;;                 ))
  ;;   (add-hook hook (lambda ()
  ;;                    (aggressive-indent-mode -1))))

  (add-to-list 'aggressive-indent-dont-electric-modes 'ruby-mode)
  (add-to-list 'aggressive-indent-dont-electric-modes 'enh-ruby-mode)
  (add-to-list 'aggressive-indent-dont-electric-modes 'inf-ruby-mode)
  (add-to-list 'aggressive-indent-dont-electric-modes 'haskell-mode)
  )



(provide 'init-my-prog-indent)

;;; init-my-prog-indent.el ends here
