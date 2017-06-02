;;; init-my-emacs-mode-line.el --- init modeline for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(setq mode-line-in-non-selected-windows t)


;;; [ window-divider-mode ]

(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  ;; (window-divider-mode 1)
  )


;;; ------------------------------------------------------
;;; separate settings for only active mode-line.

(defvar mode-line--selected-window nil
  "Variable which record mode-line current selected window.")

(defun mode-line--record-selected-window ()
  "Set mode-line selected window to current window."
  (setq mode-line--selected-window (selected-window)))

(add-hook 'post-command-hook 'mode-line--record-selected-window)

(add-hook 'buffer-list-update-hook
          (lambda () (force-mode-line-update t)))

(defun active ()
  "Indicate whether current mode-line is in current selected window."
  (eq mode-line--selected-window (selected-window)))

;;; ------------------------------------------------------

;;; define faces for mode-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface mode-line-buffer-path-face
  '((t (:inherit mode-line)))
  "Face used for the directory name part of the buffer path."
  :group 'mode-line)

(defface mode-line-buffer-project-face
  '((t (:inerit 'mode-line-buffer-path-face :bold nil)))
  "Face used for the filename "
  :group 'mode-line)

(defface mode-line-buffer-major-mode-face
  '((t (:inherit 'mode-line :bold t)))
  "Face used for the buffer's major-mode segment in mode-line."
  :group 'mode-line)

(defface mode-line-meta-face
  '((t (:inherit 'highlight)))
  "Face used for meta info in mode-line."
  :group 'mode-line)

(defface mode-line-info-face
  '((t (:inherit 'mode-line)))
  "Face used for info segments in mode-line."
  :group 'mode-line)

(defface mode-line-data-face
  '((t (:inherit 'mode-line
                 :foreground "cyan")))
  "Face used for info segments in mode-line."
  :group 'mode-line)

(defface mode-line-warn-face
  '((t (:inherit 'warning)))
  "Face used for warning segments in mode-line."
  :group 'mode-line)

(defface mode-line-urgent-face
  '((t (:inherit 'error)))
  "Face used for urgent or error info segments in mode-line."
  :group 'mode-line)

(use-package all-the-icons
  :ensure t)

;;; mode-line indicator fragments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; active window indicator
(defun *current ()
  "Display an indicator when current selected buffer."
  (if (active)
      (propertize "▌"
                  'face '(:foreground "cyan"))
    ;; (all-the-icons-faicon "chain-broken"
    ;;                       :face '(:foreground "cyan")
    ;;                       :v-adjust -0.05 :height 1.0)
    (propertize " " 'face 'variable-pitch)))

;; emacsclient indicator
(defun *emacsclient ()
  "Show whether emacsclient is active."
  (if (and (frame-parameter nil 'client) (active))
      (all-the-icons-faicon "hashtag"
                            :face 'mode-line-meta-face
                            :v-adjust -0.05)))

(defun *recursive-editing ()
  "Show current recursive editing info."
  (if (and (active)
           (not (string-empty-p (format-mode-line "%]"))))
      (concat
       (all-the-icons-octicon "pencil" :v-adjust -0.05 :face 'mode-line-warn-face)
       (propertize (format-mode-line "%]") 'face 'mode-line-warn-face))))

;;; buffer project info
(defun buffer-path-relative-to-project ()
  "Displays the buffer's full path relative to the project root.
\(includes the project root\).  Excludes the file basename."
  (if buffer-file-name
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

(defun *buffer-project ()
  "Displays `default-directory', for special buffers like the scratch buffer."
  (concat
   (all-the-icons-octicon "file-directory" :v-adjust -0.05)
   (propertize
    (concat " [" (abbreviate-file-name (projectile-project-root)) "] ")
    'face '(:height 0.8))))

(defun *projectile ()
  "Show projectile project info."
  (if (bound-and-true-p projectile-mode)
      (propertize
       (concat
        " ["
        (all-the-icons-octicon "file-directory" :v-adjust -0.05 :height 0.9)
        (propertize " " 'face 'variable-pitch)
        ;; `projectile-mode-line'
        (propertize (projectile-project-name))
        "] "
        ))))

(defun *perspeen ()
  "Show perspeen info from `perspeen-modestring'."
  (when (bound-and-true-p perspeen-modestring)
    ;; change face
    (put-text-property 0 6
                       'face (if (active) 'mode-line-info-face 'mode-line-inactive)
                       (nth 1 perspeen-modestring))
    perspeen-modestring
    ))

;;; buffer name
(defun *buffer-name ()
  "Display buffer name better."
  ;; (propertize (buffer-path-relative-to-project))
  (propertize
   (concat
    (if (not (null buffer-file-name))
        (all-the-icons-faicon "file-o" :v-adjust -0.05 :height 0.8)
      (if (derived-mode-p 'prog-mode)
          (all-the-icons-faicon "file-code-o" :v-adjust -0.05 :height 0.8)
        ))
    (propertize " " 'face 'variable-pitch)
    (propertize (buffer-name))
    )))

;;; buffer info
(defun *buffer-info ()
  "Combined information about the current buffer.

Including the current working directory, the file name, and its
state (modified, read-only or non-existent)."
  (if (active)
      (propertize
       (concat
        ;; buffer modify status
        (cond
         ((string-equal (format-mode-line "%*") "*")
          (all-the-icons-faicon "chain-broken" :v-adjust -0.0 :face 'mode-line-warn-face))
         ((string-equal (format-mode-line "%*") "-")
          (all-the-icons-faicon "link" :v-adjust -0.0))
         ((string-equal (format-mode-line "%*") "%")
          (all-the-icons-octicon "lock" :v-adjust -0.0))
         )
        ;; process buffer
        (when (null buffer-file-name)
          (all-the-icons-faicon "asterisk" :v-adjust -0.05 :face 'mode-line-data-face))
        ;; not exist file
        (when (and buffer-file-name (not (file-exists-p buffer-file-name)))
          (all-the-icons-octicon "circle-slash"
                                 :face 'mode-line-info-face
                                 :v-adjust -0.05))
        ;; remote file
        (when (and (not (null (buffer-file-name)))
                   (file-remote-p (buffer-file-name)))
          (all-the-icons-faicon "cloud-download"
                                :face 'mode-line-warn-face
                                :v-adjust -0.05))
        ;; narrow
        (when (buffer-narrowed-p)
          (all-the-icons-faicon "align-center"
                                :v-adjust -0.05
                                :face 'mode-line-data-face))
        ;; buffer size
        ;; (format-mode-line "%I")
        (propertize " " 'face 'variable-pitch)))))

;;; buffer encoding
(defun *buffer-encoding ()
  "The encoding and eol style of the buffer."
  (if (active)
      (propertize
       (concat
        (let ((eol-type (coding-system-eol-type buffer-file-coding-system)))
          (cond
           ((eq eol-type 0)
            "" ;; "LF " <-- Unix/Linux EOF
            )
           ((eq eol-type 1) "CRLF ")
           ((eq eol-type 2) "CR ")))
        (let* ((sys (coding-system-plist buffer-file-coding-system))
               (sys-name (plist-get sys :name))
               (sys-cat (plist-get sys :category)))
          (cond
           ((memq sys-cat '(coding-category-undecided coding-category-utf-8))
            "" ;; "UTF-8" <-- no need to display UTF-8.
            )
           (t
            (upcase (symbol-name sys-name)))))
        (propertize " " 'face 'variable-pitch)
        ))))

;;; bookmark
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
                                    (bookmark-set ,bookmark-name))
                                  (force-mode-line-update))))
         (propertize " " 'face 'variable-pitch))
      )))

;;; window number
;; (use-package window-numbering
;;   :ensure t)
(defun *window-number ()
  (propertize (format " %c" (+ 9311 (window-numbering-get-number)))
              'face `(:height ,(/ (* 0.90 powerline/default-height) 100.0))
              'display '(raise 0.0)))

;;; line & column position info
(defun *linum-info ()
  "Show line & column position info."
  (propertize " [%l:%c %p] "
              'face '(:family "Monospace" :height 0.8))
  )

;;; pdf-tools page position
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
        )
       'face (if (active) 'mode-line-data-face))))

;;; major-mode
(defun *major-mode ()
  "The major mode, including process, environment and text-scale info."
  (propertize
   ;; condition: (all-the-icons-auto-mode-match?)
   (if (cdr (assoc major-mode all-the-icons-mode-icon-alist))
       ;; (all-the-icons-icon-for-buffer)
       (all-the-icons-icon-for-mode major-mode :v-adjust -0.05 :height 1.0)
     ;; (all-the-icons-icon-for-file (buffer-file-name) :v-adjust -0.05 :height 1.0)
     (format-mode-line "%m" mode-name)
     )
   'help-echo (format "Major-mode: `%s`" major-mode)
   ;; 'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer))
   ))

;;; environment version info like: Python, Ruby, JavaScript,
(defun *env ()
  "Show environment version info like Python, Ruby, JavaScript etc."
  (if (active)
      (let ((env
             (cl-case major-mode
               ('clojure-mode
                (if (not (equal (cider--modeline-info) "not connected"))
                    ;; don't duplicate with `projectile'
                    (if (projectile-project-name)
                        (all-the-icons-fileicon "clj" :face '(:foreground "green"))
                      (with-current-buffer (ignore-errors (cider-current-connection))
                        (cider--project-name nrepl-project-dir))
                      )
                  (all-the-icons-fileicon "clj" :face '(:foreground "red"))))
               ('clojurescript-mode
                (if (not (equal (cider--modeline-info) "not connected"))
                    ;; don't duplicate with `projectile'
                    (if (projectile-project-name)
                        (all-the-icons-fileicon "clj" :face '(:foreground "green"))
                      (with-current-buffer (ignore-errors (cider-current-connection))
                        (cider--project-name nrepl-project-dir))
                      )
                  (all-the-icons-fileicon "clj" :face '(:foreground "red"))))
               ('ruby-mode
                (if (and global-rbenv-mode rbenv--modestring)
                    ;; `rbenv--modestring'
                    (propertize (format "%s" (rbenv--active-ruby-version)))
                  ))
               ('python-mode
                (if pyvenv-mode
                    ;; `pyvenv-mode-line-indicator' -> `pyvenv-virtual-env-name'
                    pyvenv-virtual-env-name
                  ;; conda: `conda-env-current-name'
                  ))
               ('js3-mode
                (if (and (featurep 'nvm) (not (null nvm-current-version)))
                    nvm-current-version)
                )
               )))
        (if env
            (concat "[" env "]"))
        )))

;;; vc
(defun *vc ()
  "Displays the current branch, colored based on its state."
  (when (and vc-mode buffer-file-name)
    (let ((backend (vc-backend buffer-file-name))
          (state   (vc-state buffer-file-name))
          (face    'mode-line-inactive)
          (active  (active)))
      (concat
       (propertize " " 'face 'variable-pitch)
       (case backend
         ('Git
          (all-the-icons-faicon "git" :v-adjust -0.05))
         ('SVN
          (all-the-icons-faicon "cloud" :v-adjust -0.05))
         ('Hg
          (all-the-icons-faicon "cloud" :v-adjust -0.05))
         (t
          (format "%s" vc-mode))
         )
       (propertize " " 'face 'variable-pitch)
       (cond ((memq state '(edited added))
              (if active (setq face 'mode-line-info-face))
              (all-the-icons-octicon "git-branch" :face face :v-adjust -0.05))
             ((eq state 'needs-merge)
              (if active (setq face 'mode-line-warn-face))
              (all-the-icons-octicon "git-merge" :face face :v-adjust -0.05))
             ((eq state 'needs-update)
              (if active (setq face 'mode-line-warn-face))
              (all-the-icons-octicon "arrow-down" :face face :v-adjust -0.05))
             ((memq state '(removed conflict unregistered))
              (if active (setq face 'mode-line-urgent-face))
              (all-the-icons-octicon "alert" :face face :v-adjust -0.05))
             (t
              (if active (setq face 'mode-line))
              (all-the-icons-octicon "git-compare" :face face :v-adjust -0.05)))
       (propertize " " 'face 'variable-pitch)
       (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                   'face (if active `(:foreground "yellow")))
       ))))

;;; flycheck
(defvar flycheck-current-errors)
(defvar flycheck-last-status-change)

(defun *flycheck ()
  "Show flycheck info in mode-line."
  (when (and (featurep 'flycheck) flycheck-mode)
    (let* ((text (pcase flycheck-last-status-change
                   (`finished
                    (if flycheck-current-errors
                        (let ((count (let-alist
                                         (flycheck-count-errors flycheck-current-errors)
                                       (+ (or .warning 0) (or .error 0)))))
                          (concat (all-the-icons-octicon "bug" :v-adjust -0.05
                                                         :face (if (active) '(:foreground "orange red")))
                                  (propertize
                                   ;; (format " %s issue%s" count (unless (eq 1 count) "s"))
                                   (format "%s" count))))
                      (all-the-icons-faicon "check-square" :v-adjust -0.05
                                            :face (if (active) '(:foreground "dark sea green")))))
                   (`running
                    (all-the-icons-faicon "ellipsis-h"
                                          :v-adjust -0.05
                                          :face (if (active) '(:foreground "light sea green"))))
                   (`no-checker
                    (concat (all-the-icons-octicon "alert" :v-adjust -0.05
                                                   :face (if (active) '(:foreground "dark gray")))
                            (propertize (format " %s" "No Checker"))))
                   (`not-checked
                    (concat (all-the-icons-faicon "exclamation-circle" :v-adjust -0.05
                                                  :face (if (active) '(:foreground "orange")))
                            (propertize (format " %s" " Disabled"))))
                   (`errored
                    (concat (all-the-icons-faicon "exclamation-triangle" :v-adjust -0.05
                                                  :face (if (active) '(:foreground "red")))
                            (propertize (format " %s" " Error"))))
                   (`interrupted
                    (concat (all-the-icons-faicon "ban" :v-adjust -0.05
                                                  :face (if (active) '(:foreground "dark orange")))
                            (propertize (format " %s" " Interrupted"))))
                   (`suspicious
                    (all-the-icons-faicon "question-circle" :v-adjust -0.05
                                          :face (if (active) '(:foreground "dark magenta")))))))
      (propertize text
                  'help-echo "Show Flycheck Errors"
                  'mouse-face '(:box 1)
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))
    ))

;;; build status
(defun *build-status ()
  "Show CI build status in mode-line."
  ;; `build-status-mode-line-string'
  (if (featurep 'build-status)
      (let* ((root (or (build-status--circle-ci-project-root (buffer-file-name))
                       (build-status--travis-ci-project-root (buffer-file-name))))
             (active (build-status--activate-mode))
             (status (cdr (assoc root build-status--project-status-alist)))
             (project (build-status--project (buffer-file-name))))
        (if (not (null status))
            (propertize
             (concat
              (all-the-icons-faicon "cogs" :v-adjust -0.05)
              (propertize " " 'face 'variable-pitch)
              (cond
               ((string= status "passed")
                (all-the-icons-faicon "check-circle"
                                      'face 'build-status-passed-face
                                      :v-adjust -0.05))
               ((string= status "running")
                (all-the-icons-faicon "spinner"
                                      'face 'build-status-running-face
                                      :v-adjust -0.05))
               ((string= status "failed")
                (all-the-icons-faicon "chain-broken"
                                      'face 'build-status-failed-face
                                      :v-adjust -0.05))
               ((string= status "queued")
                (all-the-icons-faicon "ellipsis-h"
                                      'face 'build-status-queued-face
                                      :v-adjust -0.05))
               (t
                (all-the-icons-faicon "question-circle-o"
                                      'face 'build-status-unknown-face
                                      :v-adjust -0.05)))
              ))
          ))))

;; region selection info
(defun *selection-info ()
  "Information about the current selection.

Such as how many characters and lines are selected, or the NxM
dimensions of a block selection."
  (when (and (active) mark-active)
    (let ((lines (count-lines (region-beginning) (region-end)))
          (words (count-words (region-end) (region-beginning))))
      (concat
       (propertize " " 'face 'variable-pitch)
       (all-the-icons-faicon "pencil-square"
                             :v-adjust -0.05
                             :face 'mode-line-data-face)
       (propertize (format " (l:%s,w:%s)" lines words)
                   'face `(:height 0.9 :foreground ,(face-foreground 'mode-line-data-face)))
       ))
    ))

;;; macro recording
(defun *macro-recording ()
  "Display current macro being recorded."
  (when (and defining-kbd-macro (active))
    (let ((separator (propertize " " 'face 'mode-line-meta-face)))
      (concat separator
              (all-the-icons-octicon "triangle-right"
                                     :face 'mode-line-meta-face
                                     :v-adjust -0.05)
              separator
              (propertize (char-to-string evil-this-macro)
                          'face 'mode-line-meta-face)
              (propertize kmacro-counter
                          'face 'mode-line-info-face)
              separator))))

;;; anzu
(defvar anzu--state)
(defvar anzu--overflow-p)
(make-variable-buffer-local 'anzu--state)

(defun *anzu ()
  "Show the match index and total number thereof.  Requires `evil-anzu'."
  (when (and (featurep 'anzu) (not (zerop anzu--total-matched)))
    (propertize
     (format " %s/%d%s "
             anzu--current-position anzu--total-matched
             (if anzu--overflow-p "+" ""))
     'face 'mode-line-meta-face)))

;;; Iedit
(defun *iedit ()
  "Show the number of iedit regions match + what match you're on."
  (when (and (boundp 'iedit-mode) iedit-mode)
    (propertize
     (let ((this-oc (let (message-log-max) (iedit-find-current-occurrence-overlay)))
           (length (or (ignore-errors (length iedit-occurrences-overlays)) 0)))
       (format " %s/%s "
               (save-excursion
                 (unless this-oc
                   (iedit-prev-occurrence)
                   (setq this-oc (iedit-find-current-occurrence-overlay)))
                 (if this-oc
                     ;; `iedit-occurrence-context-lines',
                     ;; `iedit-occurrences-overlays'.
                     (- length (-elem-index this-oc iedit-occurrence-context-lines))
                   "-"))
               length))
     'face 'mode-line-meta-face)))

;; multiple-cursors (mc/)
(use-package multiple-cursors
  :ensure t)
(defun *multiple-cursors ()
  "Show multiple-cursors indicator in mode-line."
  (if (> (mc/num-cursors) 1) ; (mc/fake-cursor-p OVERLAY)
      (propertize
       (format "[%d]" (mc/num-cursors)) ; `mc/mode-line'
       'face 'mode-line-meta-face)))

;;; Evil substitute
(defun *evil-substitute ()
  "Show number of :s match in real time."
  (when (and (evil-ex-p) (evil-ex-hl-active-p 'evil-ex-substitute))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches "
                   (count-matches pattern (car range) (cdr range))
                   evil-ex-argument)
         " ... "))
     'face (if (active) 'mode-line-meta-face))))

;; input method
(defun *input-method ()
  "Show input-method info in mode-line."
  (if (and current-input-method-title (active)) ; `set-input-method'
      (propertize (format " {%s}" current-input-method-title)
                  'face 'mode-line-meta-face)))

;; org-tree-slide slide number
(defun *org-tree-slide ()
  "Show `org-tree-slide' slide number."
  (when (bound-and-true-p org-tree-slide-mode)
    (propertize
     (concat
      (all-the-icons-faicon "file-powerpoint-o" :v-adjust -0.05)
      (format "%s" org-tree-slide--slide-number))
     'face (if (active) 'mode-line-data-face))))

;; wc-mode (word count) `wc-format-modeline-string', `wc-mode-update'.
(defun *wc-mode ()
  "Show wc-mode word count."
  (when (and (featurep 'wc-mode) wc-mode)
    (propertize (wc-format-modeline-string " Words:[%tw]")
                'face (if (active) 'mode-line-info-face))
    ))

;; mmm-mode


;; process: inferior,
(defun *process ()
  "Show `major-mode' process `mode-line-process' info."
  (when (stringp mode-line-process)
    (propertize
     (concat
      (all-the-icons-faicon "circle-o" :v-adjust -0.05)
      ;; (format-mode-line "%s")
      mode-line-process)
     'face 'mode-line-warn-face
     'help-echo "buffer-process")))

;; notifications
;; IRC
(defun *erc ()
  "Show ERC info from `erc-track-mode'."
  (if (and (featurep 'erc-track-mode) (erc-server-process-alive) erc-track-mode)
      (concat
       (all-the-icons-faicon "comments-o" :v-adjust 0.05)
       (propertize (format "[%s]" erc-modified-channels-object)
                   'face 'mode-line-data-face)
       (propertize " " 'face 'variable-pitch))
    ))

(defun *company-lighter ()
  "Show company-mode lighter from `company-lighter'."
  (if (and (boundp 'company-mode) company-mode (consp company-backend))
      (propertize
       (company--group-lighter
        (nth company-selection company-candidates) company-lighter-base)
       'face 'mode-line-data-face)))

(require 'org-clock)

(defun *org-clock ()
  "Show org-clock info."
  (when (and (active) org-clock-idle-timer)
    ;; get [0:05] from `org-clock-get-clock-string'
    (propertize
     (concat
      (all-the-icons-octicon "clock" :v-adjust 0.05)
      (propertize " " 'face 'variable-pitch)
      (format "%s"
              (org-minutes-to-clocksum-string (org-clock-get-clocked-time)))
      (propertize " " 'face 'variable-pitch))
     'face 'mode-line-data-face))
  
  ;; org-clock-today (show current org clock)
  ;; NOTE: this time is doubled on `org-clock'.
  ;; (:eval
  ;;  (when (and (org-clock-is-active) (active))
  ;;    (list
  ;;     (propertize (format " ⏰%s" org-clock-today-string)
  ;;                 'face '(:foreground "cyan")))
  ;;    ))
  )

;; update org-clock timer in mode-line after `org-clock-out-hook'.
;; fix org-clock timer does not disappear after clock out.
(add-hook 'org-clock-out-hook
          '(lambda ()
             ;; (org-clock-update-mode-line)
             (setq org-mode-line-string nil)
             (force-mode-line-update)))

(require 'org-timer)

(defun *org-timer ()
  "Show `org-timer' info in my custom mode-line."
  (if (and (active)
           (or org-timer-mode-line-timer
               org-timer-countdown-timer
               (org-at-item-timer-p)
               ))
      ;; - `org-timer-value-string'
      ;; - `org-timer-set-mode-line'
      ;; - `org-timer-mode-line-string'
      (propertize
       (concat
        (all-the-icons-material "alarm_on" :v-adjust 0.05)
        (format "%s" org-timer-mode-line-string))
       'face 'mode-line-data-face)
    )
  )

;;; Pomodoro (org-pomodoro)
(use-package org-pomodoro
  :ensure t
  :config)
(defun *pomodoro ()
  "Show pomodoro/org-pomodoro timer in custom mode-line."
  (if (and (org-pomodoro-active-p)
           (active))
      (propertize (format "%s" org-pomodoro-mode-line)
                  'face 'mode-line-data-face))
  )

;;; Current Time
(defun *time ()
  "Show current time in Emacs custom mode-line."
  (let* ((hour (string-to-number (format-time-string "%I")))
         (icon (all-the-icons-wicon (format "time-%s" hour) :height 1.3 :v-adjust 0.0)))
    (if (active)
        (concat
         (propertize (format "%s" icon)
                     'face `(:height 1.0 :family ,(all-the-icons-wicon-family))
                     'display '(raise -0.0))
         (propertize (format-time-string " %H:%M ") 'face `(:height 0.9))
         ))
    ))

(defun *space (n)
  "Add `N' spaces for custom mode-line alignment."
  (propertize (make-string n (string-to-char " ")) 'face 'variable-pitch))

;; rtags project parsing progress status
;; - rtags-remaining-jobs :: integer with count of remaining jobs for all projects
;; - rtags-last-index :: the first number in rdm's output
;; - rtags-last-total :: the second number in rdm's output, these last two are project local
;; rtags indexed file
;; - `rtags-is-indexed'
;; You have to run `rdm' with the `--progress' for this to work.
(defvar rtags-enabled)

(defun *my-rtags-modeline ()
  "Show `rtags-modeline' info in my custom mode-line."
  (if (and (active)
           (and (boundp 'rtags-enabled) rtags-enabled))
      (propertize
       (concat
        (if (not (string-empty-p (rtags-modeline)))
            (all-the-icons-faicon "paw" :v-adjust -0.05)
          (rtags-modeline))
        (if (rtags-is-indexed)
            (all-the-icons-faicon "codepen" :v-adjust -0.05))
        (propertize " " 'face 'variable-pitch))
       'face 'mode-line-info-face)))

(add-hook 'rtags-diagnostics-hook (function force-mode-line-update))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-modeline ()
  "My custom mode-line."
  `(:eval
    (let* ((meta (concat
                  (*emacsclient)
                  (*recursive-editing)
                  (*macro-recording)
                  (*selection-info)
                  (*anzu)
                  (*iedit)
                  (*multiple-cursors)
                  ;; (*evil-substitute)
                  (*input-method)
                  (*company-lighter)
                  ))
           (lhs (list
                 (*current)
                 (if (= (length meta) 0) "" meta)
                 (*buffer-info)
                 (*bookmark)
                 (*buffer-name)
                 (*buffer-encoding)
                 (*linum-info)
                 ;; (*wc-mode)
                 (*pdf-tools-page-position)
                 (*org-tree-slide)
                 (*org-clock)
                 (*org-timer)
                 (*pomodoro)
                 (*process)
                 (*my-rtags-modeline)
                 ))
           (rhs (list
                 ;; NOTE: the `mid' `format-mode-line' meet first `nil' will
                 ;; count as 0. This will cause `rhs' to 0. So make sure the
                 ;; first fragment is not `nil'. So I use " " empty string with
                 ;; 1 length.
                 " "
                 ;; (*time)
                 (*erc)
                 (*flycheck)
                 (*build-status)
                 (*vc)
                 ;; (*buffer-project)
                 (*projectile)
                 ;; (*perspeen)
                 (*major-mode)
                 (*space 1)
                 (*env)
                 (*space 1)
                 ))
           (mid (propertize
                 " "
                 'display `((space
                             :align-to (- (+ right right-fringe right-margin)
                                          ,(+ 1 (string-width (format-mode-line rhs)))))))))
      (list lhs mid rhs))))

(setq-default mode-line-format (my-modeline))


;; display time
;; (setq display-time-interval 60)
;; (setq display-time-24hr-format nil)
;; (setq display-time-format nil)
;; (setq display-time-day-and-date nil)

;; event
;; (display-time-event-handler)

;;; Mail
(setq display-time-mail-directory "~/Mails/INBOX/new/")
(setq display-time-use-mail-icon t)
;; (setq display-time-mail-file nil)
;; (setq display-time-mail-function nil)
;; (display-time-mail-check-directory)

;;; load-average
;; (setq display-time-default-load-average 0)
;; (setq display-time-load-average-threshold 0.5)

;; (display-time-mode t)


(provide 'init-my-emacs-mode-line)

;;; init-my-emacs-mode-line.el ends here
