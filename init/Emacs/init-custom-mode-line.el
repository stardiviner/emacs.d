;;; init-custom-mode-line.el --- my custom mode-line.

;;; Commentary:



;;; Code:

;;; ------------------------------------------------------
;;; Active Window
;;
;; Inspired by, but not identical to, code in `powerline'.
;; In particular we don't add anything to `focus-out-hook'
;; because that turned out to be counterproductive.

(defvar mode-line--active-window (frame-selected-window))

(defun mode-line-window-active-p ()
  "Return t if the selected window is the active window.
Or put differently, return t if the possibly only temporarily
selected window is still going to be selected when we return
to the command loop."
  (eq (selected-window) mode-line--active-window))

(defun mode-line--set-active-window (&rest _)
  (let ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq mode-line--active-window win))))

(add-hook 'window-configuration-change-hook 'mode-line--set-active-window)
(add-hook 'focus-in-hook                    'mode-line--set-active-window)
(advice-add 'handle-switch-frame :after     'mode-line--set-active-window)
(advice-add 'select-window :after           'mode-line--set-active-window)

;;; define faces for mode-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface mode-line-meta-face
  '((t (:inherit 'highlight)))
  "Face used for meta info in mode-line."
  :group 'mode-line)

(defface mode-line-info-face
  '((t (:foreground "ForestGreen")))
  "Face used for info segments in mode-line."
  :group 'mode-line)

(defface mode-line-data-face
  '((t (:foreground "DeepSkyBlue")))
  "Face used for info segments in mode-line."
  :group 'mode-line)

(defface mode-line-success-face
  '((t (:inherit 'success)))
  "Face used for info segments in mode-line."
  :group 'mode-line)

(defface mode-line-warn-face
  '((t (:inherit 'warning)))
  "Face used for warning segments in mode-line."
  :group 'mode-line)

(defface mode-line-error-face
  '((t (:inherit 'error)))
  "Face used for urgent or error info segments in mode-line."
  :group 'mode-line)

(use-package all-the-icons
  :ensure t)

;;; mode-line indicator fragments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacsclient indicator
(defun *emacsclient ()
  "Show whether emacsclient is active."
  (if (frame-parameter nil 'client)
      (all-the-icons-faicon "hashtag"
                            :face (if (mode-line-window-active-p) 'mode-line-meta-face)
                            :v-adjust -0.05)))

(defun *recursive-editing ()
  "Show current recursive editing info."
  (unless (string-empty-p (format-mode-line "%]"))
    (propertize
     (concat
      (all-the-icons-octicon "pencil" :v-adjust -0.05)
      (format-mode-line "%]"))
     'face (if (mode-line-window-active-p) 'mode-line-warn-face))))

;;; Edebug
(require 'edebug)
(defun *edebug ()
  "Show Edebug info."
  (propertize
   (if (or (bound-and-true-p edebug-mode)
           (bound-and-true-p edebug-x-mode))
       (concat
        (all-the-icons-octicon "bug" :v-adjust -0.05)
        (format "[%s]" edebug-execution-mode))
     (format ""))
   ;; 'face (if (mode-line-window-active-p) 'mode-line-data-face)
   ))

;;; buffer project info
(defun buffer-path-relative-to-project ()
  "Displays the buffer's full path relative to the project root.
\(includes the project root\).  Excludes the file basename."
  (if (bound-and-true-p buffer-file-name)
      (let* ((default-directory (f-dirname buffer-file-name))
             (buffer-path (f-relative buffer-file-name (projectile-project-root)))
             (max-length (truncate (* (window-body-width) 0.4))))
        (when (and buffer-path (not (equal buffer-path ".")))
          (if (> (length buffer-path) max-length)
              (let ((path (reverse (split-string buffer-path "/" t)))
                    (output ""))
                (when (and path (equal "" (car path)))
                  (setq path (cdr path)))
                (while (and path (<= (length output) (- max-length 4)))
                  (setq output (concat (car path) "/" output))
                  (setq path (cdr path)))
                (when path
                  (setq output (concat "../" output)))
                (when (string-suffix-p "/" output)
                  (setq output (substring output 0 -1)))
                output)
            buffer-path)))
    "%b"))

(use-package projectile
  :ensure t
  :config
  (defun *buffer-project ()
    "Displays `default-directory', for special buffers like the scratch buffer."
    (concat
     (all-the-icons-octicon "file-directory" :v-adjust -0.05)
     (propertize
      (concat " [" (abbreviate-file-name (projectile-project-root)) "] ")
      'face '(:height 0.8))))

  (defun *projectile ()
    "Show projectile project info."
    ;; `projectile-mode-line'
    (unless (and (bound-and-true-p projectile-mode)
                 (local-variable-if-set-p 'projectile-project-name))
      (make-local-variable 'projectile-project-name)
      (setq-local projectile-project-name (projectile-project-name)))
    (if projectile-project-name
        (propertize (concat
                     " "
                     (all-the-icons-faicon "folder-o" :v-adjust -0.05 :height 0.9)
                     projectile-project-name
                     " "))))
  )

;;; eyebrowse
(use-package eyebrowse
  :ensure t
  :custom (eyebrowse-keymap-prefix (kbd "C-x w"))
  :config
  (defvar-local my/eyebrowse-current-slot-tag nil)
  (defun *eyebrowse ()
    "Displays `default-directory', for special buffers like the scratch buffer."
    ;; `eyebrowse-mode-line-indicator'
    (unless (and (bound-and-true-p my/eyebrowse-current-slot-tag)
                 (local-variable-if-set-p 'my/eyebrowse-current-slot-tag))
      (let ((current-slot-tag (cadr (alist-get
                                     (eyebrowse--get 'current-slot)
                                     (eyebrowse--get 'window-configs))))
            ;; (current-slot-number (eyebrowse--get 'current-slot))
            ;; (slot-numbers (length (eyebrowse--get 'window-configs)))
            )
        (setq-local my/eyebrowse-current-slot-tag current-slot-tag)))
    (if my/eyebrowse-current-slot-tag
        (concat
         (all-the-icons-faicon "codepen" :v-adjust -0.1)
         (propertize (format " %s " my/eyebrowse-current-slot-tag)))))
  )

;; (use-package perspeen
;;   :ensure t
;;   :config
;;   (defun *perspeen ()
;;     "Show perspeen info from `perspeen-modestring'."
;;     (when (bound-and-true-p perspeen-modestring)
;;       ;; change face
;;       (put-text-property 0 6
;;                          'face (if (mode-line-window-active-p) 'mode-line 'mode-line-inactive)
;;                          (nth 1 perspeen-modestring))
;;       perspeen-modestring
;;       ))
;;   )

;; Purpose
;; (use-package purpose
;;   :ensure t
;;   :config
;;   (defun *purpose ()
;;     "Show Purpose info in custom mode-line."
;;     (if purpose-mode
;;         (concat
;;          (propertize "⊞" 'face '(:height 120))
;;          (propertize (purpose--modeline-string))
;;          (propertize " ")
;;          )))
;;   )

;;; buffer name
(defun *buffer-name ()
  "Display buffer name better."
  (concat
   " "
   (if (not (null buffer-file-name))
       (if (derived-mode-p 'prog-mode)
           (all-the-icons-faicon "file-code-o" :v-adjust -0.05 :height 0.8)
         (all-the-icons-faicon "file-o" :v-adjust -0.05 :height 0.8)))
   " "
   (propertize
    (buffer-name)
    'face 'mode-line-buffer-id))

  ;; (propertize (buffer-path-relative-to-project))
  )

;;; buffer info
(defun *buffer-info ()
  "Combined information about the current buffer.
Including the current working directory, the file name, and its
state (modified, read-only or non-existent)."
  (concat
   ;; buffer modify status
   (pcase (format-mode-line "%*")
     ;; modified
     ("*" (all-the-icons-faicon "chain-broken" :v-adjust -0.0 :face 'mode-line-warn-face))
     ;; content saved
     ("-" "")
     ;; read-only
     ("%" (all-the-icons-octicon "lock" :v-adjust -0.0 'mode-line-error-face)))
   ;; narrow
   (when (buffer-narrowed-p)
     (all-the-icons-faicon "align-center" :v-adjust -0.05 :face 'mode-line-data-face))
   ;; buffer size
   ;; (format-mode-line "%I")
   ))

;;; buffer encoding
(defun *buffer-encoding ()
  "The encoding and eol style of the buffer."
  (concat
   (let ((eol-type (coding-system-eol-type buffer-file-coding-system)))
     (cond
      ((eq eol-type 0) ; "LF " <-- Unix/Linux EOF
       "")
      ((eq eol-type 1) "CRLF ")
      ((eq eol-type 2) "CR ")))
   (let* ((sys (coding-system-plist buffer-file-coding-system))
          (sys-name (plist-get sys :name))
          (sys-cat (plist-get sys :category)))
     (cond
      ;; "UTF-8" <-- no need to display UTF-8.
      ((memq sys-cat '(coding-category-undecided coding-category-utf-8)) "")
      (t (symbol-name sys-name))))
   ))

(defun *buffer-type ()
  "Display icon for current buffer type."
  ;; process buffer
  (when (null buffer-file-name)
    (all-the-icons-faicon "asterisk" :v-adjust -0.05 :face 'mode-line-data-face))
  ;; Show an indicator for `org-src-mode' minor mode in custom mode-line.
  ;; (if (and org-src-mode (mode-line-window-active-p))
  ;;     (all-the-icons-faicon "code" :v-adjust -0.05 :face 'mode-line-data-face)
  ;;   ;; not exist file
  ;;   (when (and buffer-file-name (not (file-exists-p buffer-file-name)))
  ;;     (all-the-icons-octicon "circle-slash" :v-adjust -0.05 :face 'mode-line-error-face)))
  ;; remote file
  (when (file-remote-p (buffer-file-name))
    (all-the-icons-faicon "server" :v-adjust -0.05 :face 'mode-line-warn-face))
  ;; encrypted file
  ;; (when (string-match-p "\\.gpg" (buffer-name))
  ;;   (all-the-icons-material "enhanced_encryption" :face 'mode-line-info-face))
  ;; `isearch-mode'
  ;; (when (and isearch-mode (mode-line-window-active-p))
  ;;   (all-the-icons-faicon "search" :v-adjust -0.05 :face 'mode-line-info-face))
  )

;;; bookmark
;;; TODO: optimize the performance
(require 'bookmark)
(defun *bookmark ()
  "Show bookmark icon if current buffer is bookmarked."
  (let* ((bookmark
          (cl-find-if (lambda (it)
                        (string= (buffer-file-name)
                                 (expand-file-name (cdr (assoc 'filename it)))))
                      bookmark-alist)))
    (if bookmark
        (concat
         (propertize
          (all-the-icons-faicon "bookmark" :v-adjust -0.05 :height 0.8)
          'local-map (make-mode-line-mouse-map
                      'mouse-1 `(lambda () (interactive)
                                  (if ,(car bookmark)
                                      (bookmark-delete ,(car bookmark))
                                    (bookmark-set ,bookmark))
                                  (force-mode-line-update))))
         (propertize " "))
      )))

;;; window number
;; (use-package window-numbering
;;   :ensure t
;;   :config
;;   (defun *window-number ()
;;     (propertize (format " %c" (+ 9311 (window-numbering-get-number)))
;;                 'face `(:height ,(/ (* 0.90 powerline/default-height) 100.0))
;;                 'display '(raise 0.0)))
;;   )

;;; ace-window
(use-package ace-window
  :ensure t
  :config
  (defun *ace-window ()
    "Showing the ace-window key in the mode-line."
    (window-parameter (selected-window) 'ace-window-path))
  (add-hook 'window-configuration-change-hook 'aw-update)
  )

;;; line & column position info
(defun *linum-info ()
  "Show line & column position info."
  (propertize " [%l:%c %p] " 'face '(:height 0.8)))

;;; pdf-tools page position
(use-package pdf-tools
  :ensure t
  :config
  (defun *pdf-tools-page-position ()
    "Show current pdf-tools page current position."
    (if (eq 'pdf-view-mode major-mode)
        (propertize
         (concat
          "["
          (number-to-string (pdf-view-current-page))
          "/"
          (number-to-string (pdf-cache-number-of-pages))
          "] "
          ))))
  )

;;; major-mode
(defun *major-mode ()
  "The major mode, including process, environment and text-scale info."
  (if (cdr (assoc major-mode all-the-icons-mode-icon-alist))
      ;; (all-the-icons-icon-for-buffer)
      ;; (all-the-icons-auto-mode-match?)
      (all-the-icons-icon-for-mode major-mode :v-adjust -0.1 :height 1.0)
    ;; (all-the-icons-icon-for-file (buffer-file-name) :v-adjust -0.05 :height 1.0)
    (propertize (format-mode-line "%m" mode-name)
                'face 'mode-line-data-face)
    ))

;;; environment version info like: Python, Ruby, JavaScript,
(defun *env ()
  "Show environment version info like Python, Ruby, JavaScript etc."
  (let ((env
         (pcase major-mode
           ((or 'common-lisp-mode 'lisp-mode 'sly-mrepl-mode 'slime-repl-mode)
            (if (or
                 (and (fboundp 'sly-connected-p) (sly-connected-p))
                 (and (fboundp 'slime-connected-p) (slime-connected-p)))
                ;; TODO: use this original (:eval (sly--mode-line-format))
                (all-the-icons-fileicon "clisp"
                                        :v-adjust -0.05 :face 'mode-line-success-face)
              (concat
               (all-the-icons-fileicon "clisp"
                                       :v-adjust -0.05 :face 'mode-line-info-face)
               (all-the-icons-faicon "chain-broken"
                                     :v-adjust -0.0 :face 'mode-line-error-face))))
           ('cider-repl-mode
            ;; CIDER REPL type
            (pcase cider-repl-type
              ("clj" (all-the-icons-fileicon "clj" :face 'mode-line-success-face))
              ("cljs" (all-the-icons-fileicon "cljs" :face 'mode-line-success-face))))
           ((or 'clojure-mode 'clojurescript-mode)
            (cond
             ;; not connected
             ((not (equal (cider--modeline-info) "not connected"))
              (concat
               ;; CIDER project type
               (pcase (cider-project-type)
                 ("lein" (all-the-icons-fileicon "clj" :face 'mode-line-success-face)))
               ;; CIDER project name
               (or (cider-project-name (buffer-local-value 'nrepl-project-dir (current-buffer)))
                   (with-current-buffer (ignore-errors (cider-current-connection))
                     (format " %s" (cider--project-name nrepl-project-dir)))
                   (projectile-project-name))
               (if (bound-and-true-p cider--debug-mode)
                   (all-the-icons-faicon "wrench"
                                         :face 'mode-line-warn-face))
               (if (bound-and-true-p cider-auto-test-mode)
                   (all-the-icons-faicon "cogs"
                                         :face 'mode-line-warn-face))))
             (t (all-the-icons-faicon "chain-broken" :v-adjust -0.0 :face 'mode-line-error-face))
             ))
           ('org-mode
            (if (bound-and-true-p ob-clojure-literate-mode)
                (if (bound-and-true-p ob-clojure-literate-session)
                    (concat
                     (all-the-icons-fileicon "clj" :face 'mode-line-success-face)
                     (propertize
                      (format "%s" (replace-regexp-in-string
                                    "cider-repl " ""
                                    ob-clojure-literate-session))))
                  (all-the-icons-fileicon "clj"))))
           ((or 'ruby-mode 'inf-ruby-mode)
            (concat
             (if (ignore-errors (inf-ruby-proc))
                 (all-the-icons-alltheicon "ruby"
                                           :v-adjust -0.05 :face 'mode-line-success-face)
               (all-the-icons-faicon "chain-broken"
                                     :v-adjust -0.0 :face 'mode-line-error-face))
             (if (and (featurep 'rbenv) rbenv--modestring)
                 (propertize (format " %s" (rbenv--active-ruby-version))
                             'face 'mode-line-success-face))))
           ((or 'python-mode 'inferior-python-mode)
            (concat
             (if (get-buffer-process (if (eq major-mode 'inferior-python-mode)
                                         (current-buffer)
                                       "*Python*"))
                 (all-the-icons-alltheicon "python"
                                           :v-adjust -0.05 :face 'mode-line-success-face)
               (all-the-icons-faicon "chain-broken"
                                     :v-adjust -0.0 :face 'mode-line-error-face))
             (if (and (featurep 'pyvenv-mode) pyvenv-mode)
                 ;; `pyvenv-mode-line-indicator' -> `pyvenv-virtual-env-name'
                 (propertize (format " %s" pyvenv-virtual-env-name)
                             'face 'mode-line-success-face)
               ;; conda: `conda-env-current-name'
               )))
           ((or 'js-mode 'js2-mode 'js3-mode)
            (if (and (featurep 'nvm) (not (null nvm-current-version)))
                nvm-current-version))
           ((or 'sql-interactive-mode)
            (if (bound-and-true-p sql-product)
                (format "%s" sql-product)))
           ((or 'image-mode)
            (if (bound-and-true-p image-type)
                (format "%s" image-type)))
           )))
    (if env
        (concat "[" env "] ")
      (propertize " "))
    ))

;;; vc
(defun *vc ()
  "Displays the current branch, colored based on its state."
  (when (bound-and-true-p vc-mode)
    (let ((backend (vc-backend buffer-file-name))
          (state   (vc-state buffer-file-name))
          (face    'mode-line-inactive)
          (active  (mode-line-window-active-p)))
      (concat
       (case backend
         ('Git
          (all-the-icons-faicon "git" :v-adjust -0.05))
         ('SVN
          (all-the-icons-faicon "cloud" :v-adjust -0.05))
         ('Hg
          (all-the-icons-faicon "cloud" :v-adjust -0.05))
         (t (format "%s" vc-mode)))
       (propertize "^")
       (cond ((memq state '(edited added))
              (if active (setq face 'mode-line))
              (all-the-icons-octicon "git-branch" :face face :v-adjust -0.05))
             ((eq state 'needs-merge)
              (if active (setq face 'mode-line-warn-face))
              (all-the-icons-octicon "git-merge" :face face :v-adjust -0.05))
             ((eq state 'needs-update)
              (if active (setq face 'mode-line-warn-face))
              (all-the-icons-octicon "arrow-down" :face face :v-adjust -0.05))
             ((memq state '(removed conflict unregistered))
              (if active (setq face 'mode-line-error-face))
              (all-the-icons-octicon "alert" :face face :v-adjust -0.05))
             (t
              (if active (setq face 'mode-line))
              (all-the-icons-octicon "git-compare" :face face :v-adjust -0.05)))
       (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                   'face (if active `mode-line-data-face))
       ))))

;;; flycheck
(use-package flycheck
  :ensure t
  :config
  (defun *flycheck ()
    "Show flycheck info in mode-line."
    (when (bound-and-true-p flycheck-mode)
      (let* ((text (pcase flycheck-last-status-change
                     (`finished
                      (if flycheck-current-errors
                          (let ((count (let-alist
                                           (flycheck-count-errors flycheck-current-errors)
                                         (+ (or .warning 0) (or .error 0)))))
                            (concat (all-the-icons-octicon "bug" :v-adjust -0.05
                                                           :face (if (mode-line-window-active-p) 'mode-line-error-face))
                                    (propertize
                                     ;; (format " %s issue%s" count (unless (eq 1 count) "s"))
                                     (format "%s" count))))
                        (all-the-icons-faicon "check-square" :v-adjust -0.05
                                              :face (if (mode-line-window-active-p) 'mode-line-info-face))))
                     (`running
                      (propertize (all-the-icons-faicon "ellipsis-h"
                                                        :v-adjust -0.05
                                                        :face (if (mode-line-window-active-p) 'mode-line-data-face))
                                  'help-echo "Flycheck running ..."))
                     (`no-checker
                      (propertize (all-the-icons-octicon "alert" :v-adjust -0.05
                                                         :face (if (mode-line-window-active-p) 'mode-line-warn-face))
                                  'help-echo "No Checker"))
                     (`not-checked
                      (propertize (all-the-icons-faicon "exclamation-circle" :v-adjust -0.05
                                                        :face (if (mode-line-window-active-p) 'mode-line-warn-face))
                                  'help-echo "Not Checked"))
                     (`errored
                      (propertize (all-the-icons-faicon "exclamation-triangle" :v-adjust -0.05
                                                        :face (if (mode-line-window-active-p) 'mode-line-warn-face))
                                  'help-echo "Errored"))
                     (`interrupted
                      (propertize (all-the-icons-faicon "ban" :v-adjust -0.05
                                                        :face (if (mode-line-window-active-p) 'mode-line-warn-face))
                                  'help-echo "Interrupted"))
                     (`suspicious
                      (propertize (all-the-icons-faicon "question-circle" :v-adjust -0.05
                                                        :face (if (mode-line-window-active-p) 'mode-line-warn-face))
                                  'help-echo "Suspicious")))))
        (propertize (concat text " ")
                    'mouse-face '(:box 1)
                    'local-map (make-mode-line-mouse-map
                                'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))
      ))
  )

;;; build status
;; (use-package build-status
;;   :ensure t
;;   :config
;;   (defun *build-status ()
;;     "Show CI build status in mode-line."
;;     ;; `build-status-mode-line-string'
;;     (if (featurep 'build-status)
;;         (let* ((root (or (build-status--circle-ci-project-root (buffer-file-name))
;;                          (build-status--travis-ci-project-root (buffer-file-name))))
;;                (active (build-status--activate-mode))
;;                (status (cdr (assoc root build-status--project-status-alist)))
;;                (project (build-status--project (buffer-file-name))))
;;           (if (not (null status))
;;               (propertize
;;                (concat
;;                 (all-the-icons-faicon "cogs" :v-adjust -0.05)
;;                 (propertize " ")
;;                 (cond
;;                  ((string= status "passed")
;;                   (all-the-icons-faicon "check-circle"
;;                                         'face 'build-status-passed-face
;;                                         :v-adjust -0.05))
;;                  ((string= status "running")
;;                   (all-the-icons-faicon "spinner"
;;                                         'face 'build-status-running-face
;;                                         :v-adjust -0.05))
;;                  ((string= status "failed")
;;                   (all-the-icons-faicon "chain-broken"
;;                                         'face 'build-status-failed-face
;;                                         :v-adjust -0.05))
;;                  ((string= status "queued")
;;                   (all-the-icons-faicon "ellipsis-h"
;;                                         'face 'build-status-queued-face
;;                                         :v-adjust -0.05))
;;                  (t
;;                   (all-the-icons-faicon "question-circle-o"
;;                                         'face 'build-status-unknown-face
;;                                         :v-adjust -0.05)))
;;                 ))
;;             ))))
;;   )

;; region selection info
(defun *selection-info ()
  "Information about the current selection.
Such as how many characters and lines are selected, or the NxM
dimensions of a block selection."
  (when mark-active
    (let ((lines (count-lines (region-beginning) (region-end)))
          (words (count-words (region-end) (region-beginning))))
      (propertize
       (concat
        " "
        (all-the-icons-faicon "pencil-square" :v-adjust -0.05)
        (format " (l:%s,w:%s)" lines words))
       'face (if (mode-line-window-active-p) 'mode-line-error-face)))))

;;; macro recording
(defun *macro-recording ()
  "Display current macro being recorded."
  (when (and defining-kbd-macro)
    (let ((separator (propertize " " 'face 'mode-line-meta-face)))
      (propertize
       (concat separator
               (all-the-icons-octicon "triangle-right" :v-adjust -0.05
                                      :face 'mode-line-meta-face)
               separator
               (if (bound-and-true-p evil-this-macro)
                   (char-to-string evil-this-macro))
               (number-to-string kmacro-counter)
               separator)
       'face (if (mode-line-window-active-p) 'mode-line-meta-face)))))

;;; anzu
(use-package anzu
  :ensure t
  :preface
  (defvar anzu--state nil)
  (defvar anzu--overflow-p nil)
  (make-local-variable 'anzu--state)
  :config
  (defun *anzu ()
    "Show the match index and total number thereof.  Requires `evil-anzu'."
    (unless (and (bound-and-true-p anzu--total-matched) (zerop anzu--total-matched))
      (propertize
       (format " %s/%d%s "
               anzu--current-position anzu--total-matched
               (if anzu--overflow-p "+" ""))
       'face (if (mode-line-window-active-p) 'mode-line-meta-face))))
  )

;;; Iedit
(use-package iedit
  :ensure t
  :config
  (defun *iedit ()
    "Show the number of iedit regions match + what match you're on."
    (when (bound-and-true-p iedit-mode)
      (propertize
       (let ((this-oc (let (message-log-max) (iedit-find-current-occurrence-overlay)))
             (length (or (ignore-errors (length iedit-occurrences-overlays)) 0)))
         (format " %s/%s "
                 (save-excursion
                   (unless this-oc
                     (iedit-prev-occurrence)
                     (setq this-oc (iedit-find-current-occurrence-overlay)))
                   (if this-oc
                       (- length (-elem-index this-oc iedit-occurrences-overlays))
                     "-"))
                 length))
       'face 'mode-line-meta-face)))
  )

;; multiple-cursors (mc/)
(use-package multiple-cursors
  :ensure t
  :config
  (defun *multiple-cursors ()
    "Show multiple-cursors indicator in mode-line."
    (if (> (mc/num-cursors) 1) ; (mc/fake-cursor-p OVERLAY)
        (propertize
         ;; `mc/mode-line'
         (format "[%d]" (mc/num-cursors))
         'face 'mode-line-meta-face))))

;; input method
(defun *input-method ()
  "Show input-method info in mode-line."
  (if (bound-and-true-p current-input-method-title)
      ;; `set-input-method'
      (propertize (format " {%s}" current-input-method-title)
                  'face 'mode-line-meta-face)))

;; org-tree-slide slide number
(use-package org-tree-slide
  :ensure t
  :config
  (defun *org-tree-slide ()
    "Show `org-tree-slide' slide number."
    (when (bound-and-true-p org-tree-slide-mode)
      (propertize
       (concat
        (all-the-icons-faicon "file-powerpoint-o" :v-adjust -0.05)
        (format "%s" org-tree-slide--slide-number))
       'face (if (mode-line-window-active-p) 'mode-line-data-face))))
  )

;; wc-mode (word count) `wc-format-modeline-string', `wc-mode-update'.
(use-package wc-mode
  :ensure t
  :config
  (defun *wc-mode ()
    "Show wc-mode word count."
    (when (bound-and-true-p wc-mode)
      (propertize (wc-format-modeline-string " Words:[%tw]")))
    ))

;;; org-noter
(use-package org-noter
  :ensure t
  :config
  (defun *org-noter ()
    "Display org-noter notes count."
    (if (bound-and-true-p org-noter-doc-mode)
        (org-noter--mode-line-text))))

;; mmm-mode


;; process: inferior,
(defun *mode-line-process ()
  "Show `major-mode' process `mode-line-process' info."
  (if (stringp mode-line-process)
      (propertize
       (concat
        (propertize " ")
        (all-the-icons-faicon "spinner" :v-adjust -0.05)
        ;; (format-mode-line "%s")
        mode-line-process)
       'face 'mode-line-data-face
       'help-echo "buffer-process")))

;;; spinner
(use-package spinner
  :ensure t
  :config
  (defun *spinner ()
    "Show current buffer local spinner with in custom mode-line."
    (if (bound-and-true-p spinner-current)
        '(:eval (spinner-print spinner-current)))))

;; notifications
;; IRC
(use-package erc
  :ensure t
  :config
  (defun *erc ()
    "Show ERC info from `erc-track-mode'."
    (if (and (erc-server-process-alive)
             ;; (erc-server-buffer-live-p)
             (string-empty-p erc-modified-channels-object))
        (propertize
         (concat
          (all-the-icons-faicon "comments-o" :v-adjust 0.05)
          (format "%s" erc-modified-channels-object))
         'face (if (mode-line-window-active-p) 'mode-line-error-face))
      )))

(use-package company
  :ensure t
  :config
  (defun *company-lighter ()
    "Show company-mode lighter from `company-lighter'."
    (if (and (bound-and-true-p company-mode) (consp company-backend))
        (propertize
         (company--group-lighter
          (nth company-selection company-candidates) company-lighter-base)
         'face 'mode-line-data-face))))


(require 'org-clock)
(defun *org-clock ()
  "Show org-clock info."
  (when (and (org-clock-is-active)
             org-clock-idle-timer
             (mode-line-window-active-p))
    (propertize
     (concat
      " "
      (all-the-icons-faicon "hourglass-half" :v-adjust -0.05 :height 0.8)
      " "
      ;; get [0:05] from `org-clock-get-clock-string'
      (format "%s" (org-duration-from-minutes (org-clock-get-clocked-time)))
      ;; get clocking task title
      (format " %s" (s-truncate 30 org-clock-heading))
      " "))))

;; update org-clock timer in mode-line after `org-clock-out-hook'.
;; fix org-clock timer does not disappear after clock out.
(add-hook 'org-clock-out-hook
          '(lambda ()
             ;; (org-clock-update-mode-line)
             (setq org-mode-line-string nil)
             (force-mode-line-update)))


;;; [ org-clock-today ] -- show the total clocked time of the current day in the mode line.

;; (use-package org-clock-today
;;   :ensure t
;;   :config
;;   (org-clock-today-mode 1)
;;  
;;   (defun *org-clock-today ()
;;   "Show `org-clock-today' current org clock)."
;;   (when (and (mode-line-window-active-p)
;;              (org-clock-is-active))
;;     (propertize
;;      (concat
;;       (format "/")
;;       (propertize " ")
;;       (all-the-icons-material "timelapse")
;;       (format "%s" org-clock-today-string))
;;      'face 'mode-line-warn-face)
;;     ))
;;   )


(require 'org-timer)
(defun *org-timer ()
  "Show `org-timer' info in my custom mode-line."
  (if (or
       (bound-and-true-p org-timer-mode-line-timer)
       (bound-and-true-p org-timer-countdown-timer)
       (bound-and-true-p org-timer-mode-line-string))
      ;; - `org-timer-value-string'
      ;; - `org-timer-set-mode-line'
      ;; - `org-timer-mode-line-string'
      (propertize
       (concat
        (all-the-icons-faicon "hourglass-half" :v-adjust -0.05 :height 0.9)
        (format "%s" org-timer-mode-line-string))
       'face 'mode-line-data-face)))

;;; Pomodoro (org-pomodoro)
(use-package org-pomodoro
  :ensure t
  :config
  (setq org-pomodoro-format
        (concat
         (all-the-icons-faicon "refresh"
                               :v-adjust -0.05 :height 0.9 :face 'mode-line-data-face)
         (propertize " %s")))
  (setq org-pomodoro-short-break-format
        (concat
         (all-the-icons-faicon "pause-circle-o"
                               :v-adjust -0.05 :height 0.9 :face 'mode-line-data-face)
         (propertize " %s")))
  (setq org-pomodoro-long-break-format
        (concat
         (all-the-icons-faicon "pause-circle"
                               :v-adjust -0.05 :height 0.9 :face 'mode-line-data-face)
         (propertize " %s")))
  (defun *pomodoro ()
    "Show pomodoro/org-pomodoro timer in custom mode-line."
    (if (org-pomodoro-active-p)
        (propertize (format "%s" org-pomodoro-mode-line)
                    'face (if (mode-line-window-active-p) 'mode-line-data-face))))
  )

;;; Current Time
(defun *time ()
  "Show current time in Emacs custom mode-line."
  (let* ((hour (string-to-number (format-time-string "%I")))
         (icon (all-the-icons-wicon (format "time-%s" hour) :height 1.3 :v-adjust 0.0)))
    (if (mode-line-window-active-p)
        (concat
         (propertize (format "%s" icon)
                     'face `(:height 1.0 :family ,(all-the-icons-wicon-family))
                     'display '(raise -0.0))
         (propertize (format-time-string " %H:%M ") 'face `(:height 0.9))
         ))))

;;; [ keycast ]
(use-package keycast
  :ensure t
  :preface
  (setq keycast-window-predicate 'mode-line-window-active-p)
  (setq keycast-separator-width 0)
  :config
  (add-to-list 'keycast-substitute-alist '(self-insert-command . nil))
  (add-to-list 'keycast-substitute-alist '(org-self-insert-command . nil))
  (add-to-list 'keycast-substitute-alist '(next-line . nil))
  (add-to-list 'keycast-substitute-alist '(previous-line . nil))
  (add-to-list 'keycast-substitute-alist '(forward-char . nil))
  (add-to-list 'keycast-substitute-alist '(backward-char . nil))
  (add-to-list 'keycast-substitute-alist '(move-beginning-of-line . nil))
  (add-to-list 'keycast-substitute-alist '(move-end-of-line . nil))
  (defun *keycast ()
    "Show keycast in custom mode-line."
    (let ((screen-half-width (- (/ (/ (display-pixel-width) 2) 10) 3)))
      (when (and (> screen-half-width 80) (mode-line-window-active-p))
        (ignore-errors
          mode-line-keycast))))
  (defun my:keycast-faces-setup (theme)
    (set-face-attribute 'keycast-key nil
                        :inherit 'mode-line
                        :height 1.0
                        ;; :weight 'bold
                        :background (face-background 'mode-line)
                        :foreground (face-foreground 'mode-line)
                        :box (face-attribute 'mode-line :box))
    (set-face-attribute 'keycast-command nil
                        :bold nil))
  (add-hook 'circadian-after-load-theme-hook #'my:keycast-faces-setup)
  (my:keycast-faces-setup nil)
  )

(defun *space (n)
  "Add `N' spaces for custom mode-line alignment."
  (propertize (make-string n (string-to-char " "))))

;; rtags project parsing progress status
;; - rtags-remaining-jobs :: integer with count of remaining jobs for all projects
;; - rtags-last-index :: the first number in rdm's output
;; - rtags-last-total :: the second number in rdm's output, these last two are project local
;; rtags indexed file
;; - `rtags-is-indexed'
;; You have to run `rdm' with the `--progress' for this to work.

;; (use-package rtags
;;   :ensure t
;;   :defines rtags-enabled
;;   :config
;;   ;; FIXME: `*rtags-mode-line' caused `beacon-mode' blink does not fade off.
;;   (defun *rtags-mode-line ()
;;     "Show `rtags-mode-line' info in my custom mode-line."
;;     (if (bound-and-true-p rtags-enabled)
;;         (propertize
;;          (concat
;;           (if (not (string-empty-p (rtags-mode-line)))
;;               (all-the-icons-faicon "paw" :v-adjust -0.05)
;;             (rtags-mode-line))
;;           (if (rtags-is-indexed)
;;               (all-the-icons-faicon "codepen" :v-adjust -0.05))
;;           " ")
;;          )))
;;   ;; (add-hook 'rtags-diagnostics-hook #'force-mode-line-update)
;;   )

;;; [ Email ]
;;; mu4e
(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :load (mu4e mu4e-contrib)
  :ensure mu4e-alert
  :init (require 'mu4e-alert)
  :config
  (defun *mu4e ()
    "Show `mu4e-alert' new messages count in custom mode-line."
    (if (member major-mode '(mu4e-main-mode mu4e-headers-mode mu4e-view-mode))
        (concat
		     (propertize
		      (or (ignore-errors (mu4e~quote-for-modeline mu4e~headers-last-query)) ""))
		     " "
		     (mu4e-context-label)
		     (if (and mu4e-display-update-status-in-modeline
			            (buffer-live-p mu4e~update-buffer)
			            (process-live-p (get-buffer-process mu4e~update-buffer)))
			       (propertize " (updating)" 'face 'mode-line-info-face)
			     "")
         (if (bound-and-true-p mu4e-alert-mode-line)
             (propertize
              (format "%s" mu4e-alert-mode-line)
              'face 'mode-line-data-face)
           "")
         )))
  )

;;; Gnus
(use-package gnus
  :ensure t
  :config
  (defun *gnus ()
    "Show `gnus' new messages count in custom mode-line."
    ;; (if (and (mode-line-window-active-p) )
    ;;     (concat
    ;;      (all-the-icons-faicon "mail" :v-adjust -0.05)
    ;;      ;; (propertize gnus-summary-mode-line-format)
    ;;      ;; (gnus-set-mode-line 'summary)
    ;;      ))
    )
  )

;;; GitHub Notifications (Participating)
(use-package ghub+
  :ensure t
  :config
  (defvar github-notifications-number 0)

  (defun github-fetch-notifications ()
    (setq github-notifications-number
          (length
           ;; check out documentation of `ghubp-get-notifications'.
           (ignore-errors
             (ghubp-get-notifications :participating "true"))
           ;; (ghub-get "/notifications" '((:participating . "true")))
           )))
  
  (run-with-timer 10 (* 10 60) 'github-fetch-notifications)
  
  (defun *github-notifications ()
    (if (> github-notifications-number 0)
        (propertize
         (concat
          (all-the-icons-faicon "github" :v-adjust 0.05)
          (format " %s " github-notifications-number))
         'face 'mode-line-data-face)))
  
  (defun github-open-notifications-participating ()
    "Open GitHub Notifications/Participating page."
    (interactive)
    (browse-url "https://github.com/notifications/participating"))
  (unless (boundp 'prog-vcs-prefix)
    (define-prefix-command 'prog-vcs-prefix))
  (define-key prog-vcs-prefix (kbd "N") 'github-open-notifications-participating)
  )

(use-package proxy-mode
  :ensure t
  :config
  (defun *proxy-mode ()
    (if (bound-and-true-p proxy-mode-proxy-type)
        (propertize
         (concat
          (all-the-icons-faicon "compress" :v-adjust -0.05)
          (format " %s " proxy-mode-proxy-type))))))

;;; Lunar Sunrise/Sunset
;; (use-package celestial-mode-line
;;   :ensure t
;;   :config
;;   (celestial-mode-line-start-timer)
;;   (defun *lunar-sun ()
;;     (if (mode-line-window-active-p)
;;         celestial-mode-line-string))
;;   )

;;; EMMS
(use-package emms
  :ensure t
  :load emms-mode-line
  :config
  (emms-mode-line 1)
  (setq emms-mode-line-format "%s")
  (defun *emms ()
    (if (and (bound-and-true-p emms-player-playing-p)
             (mode-line-window-active-p))
        (let* ((track (emms-playlist-current-selected-track))
               ;; (description (emms-track-description (emms-playlist-current-selected-track)))
               (title (cdr (assoc 'info-title track))))
          (when emms-player-playing-p
            (concat
             (all-the-icons-faicon "music" :v-adjust -0.05)
             " "
             (format
              emms-mode-line-format
              (s-truncate 20 title))
             ;; emms-playing-time-string
             " "
             ))
          )))
  ;; [ emms-mode-line-cycle ]
  ;; :ensure emms-mode-line-cycle
  ;; :load (emms-mode-line-icon)
  ;; :init (emms-playing-time 1) (emms-mode-line-cycle 1)
  ;; :config
  ;; (setq emms-mode-line-cycle-use-icon-p t
  ;;       emms-mode-line-cycle-max-width 25)
  ;; (defun *emms ()
  ;;   (emms-mode-line-cycle--icon-function))
  )

;;; TRAMP
(defun *tramp ()
  "Show TRAMP info in custom mode-line."
  (when (and (not (null (buffer-file-name)))
             (file-remote-p (buffer-file-name)))
    (concat
     (all-the-icons-faicon "server"
                           :face 'mode-line-warn-face
                           :v-adjust -0.05)
     (propertize " "))))

;;; `copy-file-on-save'
(use-package copy-file-on-save
  :ensure t
  :config
  ;; show this segment in custom mode-line.
  (defun *copy-file-on-save ()
    "Use `copy-file-on-save-lighter' in custom mode-line."
    (if (bound-and-true-p copy-file-on-save-mode)
        copy-file-on-save-lighter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun custom-mode-line ()
  "My custom mode-line."
  `(:eval
    (let* ((meta (concat
                  (*emacsclient)
                  (*tramp)
                  ;; (*edebug)
                  (*recursive-editing)
                  (*macro-recording)
                  ;; (*selection-info)
                  (*anzu)
                  (*iedit)
                  ;; (*multiple-cursors)
                  ;; (*evil-substitute)
                  (*input-method)
                  ))
           (lhs (list
                 ;; (propertize "•")
                 ;; (*window-number)
                 ;; (*ace-window)
                 (if (= (length meta) 0) "" meta)
                 (*buffer-info)
                 ;; (*buffer-type)
                 ;; (*bookmark)
                 (*buffer-name)
                 ;; mode-line-frame-identification
                 (*buffer-encoding)
                 ;; (*linum-info)
                 ;; (*wc-mode)
                 (*pdf-tools-page-position)
                 (*org-tree-slide)
                 (*org-noter)
                 (*org-clock)
                 ;; (*org-clock-today)
                 (*org-timer)
                 (*pomodoro)
                 ;; (*keycast)
                 (*mode-line-process)
                 (*spinner)
                 (*copy-file-on-save)
                 ;; (*rtags-mode-line)
                 ))
           (rhs (list
                 ;; NOTE: the `mid' `format-mode-line' meet first `nil' will
                 ;; count as 0. This will cause `rhs' to 0. So make sure the
                 ;; first fragment is not `nil'. So I use " " empty string with
                 ;; 1 length.
                 " "
                 ;; (*company-lighter)
                 ;; (*time)
                 ;; (*lunar-sun)
                 (*erc)
                 (*emms)
                 (*mu4e)
                 ;; (*gnus)
                 (*github-notifications)
                 (*flycheck)
                 ;; (*build-status)
                 (*vc)
                 ;; (*buffer-project)
                 (*projectile)
                 (*eyebrowse)
                 ;; (*perspeen)
                 ;; (*purpose)
                 (*major-mode)
                 ;; (propertize (format-mode-line "%m" mode-name))
                 (*env)
                 (*space 3)
                 ))
           (mid (propertize
                 " "
                 ;; NOTE: `all-the-icons-faicon' affect the length calculating of mode-line fragments.
                 'display `((space
                             :align-to (- (+ right right-fringe right-margin)
                                          ,(+ 1 (string-width (format-mode-line rhs)))))))))
      (list lhs mid rhs))))

(setq-default mode-line-format (custom-mode-line))


;; (set-face-attribute 'mode-line nil
;;                     ;; :box `(:line-width 2 :color ,(color-darken-name (face-background 'default) 3))
;;                     :height 100
;;                     )
;; (set-face-attribute 'mode-line-inactive nil
;;                     ;; :box `(:line-width 1 :color ,(color-darken-name (face-background 'default) 3))
;;                     :height 100
;;                     )


;;; display time
;; (setq display-time-interval 60)
;; (setq display-time-24hr-format nil)
;; (setq display-time-format nil)
;; (setq display-time-day-and-date nil)
;;
;;; event
;; (display-time-event-handler)
;;
;;; Mail
;; (setq display-time-mail-directory "~/Mails/INBOX/new/")
;; (setq display-time-use-mail-icon t)
;; (setq display-time-mail-file nil)
;; (setq display-time-mail-function nil)
;; (display-time-mail-check-directory)
;;
;;; load-average
;; (setq display-time-default-load-average 0)
;; (setq display-time-load-average-threshold 0.5)
;;
;; (display-time-mode t)



(provide 'init-custom-mode-line)

;;; init-custom-mode-line.el ends here
