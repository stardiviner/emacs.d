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

;;; mode-line indicator fragments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; active window indicator
(defun *current ()
  "Display an indicator when current selected buffer."
  (if (active)
      (concat
       ;; (propertize " " 'face 'variable-pitch)
       (propertize "▌"
                   'face '(:foreground "cyan"))
       (all-the-icons-faicon "chain-broken"
                             :face '(:foreground "cyan")
                             :v-adjust -0.05)
       )
    (propertize " " 'face 'variable-pitch)))

;; emacsclient indicator
(defun *emacsclient ()
  "Show whether emacsclient is active."
  (if (and (frame-parameter nil 'client) (active))
      (all-the-icons-faicon "hashtag"
                            :face 'mode-line-meta-face
                            :v-adjust -0.05)))

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
        (all-the-icons-octicon "file-directory" :v-adjust -0.05)
        " "
        (propertize (projectile-project-name) ; `projectile-mode-line'
                    'face (if (active) 'mode-line-info-face))
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
        (all-the-icons-faicon "file-o" :v-adjust -0.05)
      (if (derived-mode-p 'prog-mode)
          (all-the-icons-faicon "file-code-o" :v-adjust -0.05)
        ))
    (propertize " " 'face 'variable-pitch)
    (propertize (buffer-name)
                'face (if (active) 'mode-line-buffer-path-face))
    )))

;;; buffer info
(defun *buffer-info ()
  "Combined information about the current buffer.

Including the current working directory, the file name, and its
state (modified, read-only or non-existent)."
  (if (active)
      (propertize
       (concat
        (if buffer-read-only
            (all-the-icons-octicon "lock"
                                   :face 'mode-line-urgent-face
                                   :v-adjust -0.05)
          (when (and (not (null buffer-file-name))
                     (buffer-modified-p))
            (all-the-icons-faicon "pencil-square-o"
                                  :face 'mode-line-warn-face
                                  :v-adjust -0.1)))
        (when (null buffer-file-name) ; process buffer
          (all-the-icons-faicon "asterisk" :v-adjust -0.05 :face 'mode-line-data-face))
        (when (and buffer-file-name (not (file-exists-p buffer-file-name))) ; not exist file
          (all-the-icons-octicon "circle-slash"
                                 :face 'mode-line-info-face
                                 :v-adjust -0.05))
        (when (and (not (null (buffer-file-name))) ; remote file
                   (file-remote-p (buffer-file-name)))
          (all-the-icons-faicon "cloud-download"
                                :face 'mode-line-warn-face
                                :v-adjust -0.05))
        (when (buffer-narrowed-p) ; narrow
          (all-the-icons-faicon "align-center"
                                :v-adjust -0.05
                                :face 'mode-line-data-face))
        (propertize " " 'face 'variable-pitch)))))

;;; buffer encoding
(defun *buffer-encoding ()
  "The encoding and eol style of the buffer."
  (if (active)
      (propertize
       (concat
        (let ((eol-type (coding-system-eol-type buffer-file-coding-system)))
          (cond ((eq eol-type 0) "LF ")
                ((eq eol-type 1) "CRLF ")
                ((eq eol-type 2) "CR ")))
        (let* ((sys (coding-system-plist buffer-file-coding-system))
               (sys-name (plist-get sys :name))
               (sys-cat (plist-get sys :category)))
          (cond ((memq sys-cat '(coding-category-undecided coding-category-utf-8))
                 "UTF-8")
                (t (upcase (symbol-name sys-name)))))
        " ")
       'face 'mode-line-info-face)))

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
   (concat
    (if (and (featurep 'all-the-icons)
             (buffer-file-name)
             (all-the-icons-auto-mode-match?))
        ;; (all-the-icons-icon-for-buffer)
        ;; (all-the-icons-icon-for-mode major-mode :v-adjust -0.05)
        (all-the-icons-icon-for-file (buffer-file-name) :v-adjust -0.05)
      (format-mode-line "%s" mode-name) ; FIXME:
      )
    (propertize "  "
                'face 'variable-pitch))
   'face (if (active) 'mode-line-buffer-major-mode-face)))

;;; environment version info like: Python, Ruby, JavaScript,
(defun *env ()
  "Show environment version info like Python, Ruby, JavaScript etc."
  (if (active)
      (let ((env (cl-case major-mode
                   ('clojure-mode
                    (if (not (equal (cider--modeline-info) "not connected"))
                        (cider--project-name nrepl-project-dir)))
                   ('enh-ruby-mode
                    (if global-rbenv-mode
                        (rbenv--active-ruby-version) ; `rbenv--modestring'
                      ))
                   ('python-mode
                    (if pyvenv-mode
                        ;; `pyvenv-mode-line-indicator' -> `pyvenv-virtual-env-name'
                        pyvenv-virtual-env-name
                      ;; conda: `conda-env-current-name'
                      ))
                   ('js3-mode
                    ;; FIXME: `nvm-current-version' is `nil'.
                    ;; (if (featurep 'nvm)
                    ;;     nvm-current-version)
                    )
                   )))
        (if env
            (propertize (concat " [" env "] ")
                        'face 'mode-line-info-face))
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
          (all-the-icons-faicon "git" :v-adjust -0.05)))
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
              (all-the-icons-octicon "git-branch" :face face :v-adjust -0.05)))
       " "
       (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                   'face (if active face))
       ))))

;;; flycheck
(defun *flycheck ()
  "Show flycheck info in mode-line."
  (when (and (featurep 'flycheck) flycheck-mode)
    (let* ((text (pcase flycheck-last-status-change
                   (`finished
                    (if flycheck-current-errors
                        (let ((count (let-alist
                                         (flycheck-count-errors flycheck-current-errors)
                                       (+ (or .warning 0) (or .error 0)))))
                          (concat (all-the-icons-octicon "bug" :v-adjust -0.05)
                                  (propertize
                                   (format " %s issue%s" count (unless (eq 1 count) "s")))))
                      (all-the-icons-faicon "check-square" :v-adjust -0.05)))
                   (`running
                    (all-the-icons-faicon "ellipsis-h" :v-adjust -0.05))
                   (`no-checker
                    (concat (all-the-icons-octicon "alert" :v-adjust -0.05)
                            (propertize (format " %s" "No Checker"))))
                   (`not-checked
                    (concat (all-the-icons-faicon "exclamation-circle" :v-adjust -0.05)
                            (propertize (format " %s" " Disabled"))))
                   (`errored
                    (concat (all-the-icons-faicon "exclamation-triangle" :v-adjust -0.05)
                            (propertize (format " %s" " Error"))))
                   (`interrupted
                    (concat (all-the-icons-faicon "ban" :v-adjust -0.05)
                            (propertize (format " %s" " Interrupted"))))
                   (`suspicious
                    (all-the-icons-faicon "question-circle" :v-adjust -0.05)))))
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
      ))
  )

;; selection info
(defun *selection-info ()
  "Information about the current selection.

Such as how many characters and lines are selected, or the NxM
dimensions of a block selection."
  (when (and (active) mark-active)
    (let ((words (count-lines (region-beginning) (region-end)))
          (chars (count-words (region-end) (region-beginning))))
      (concat
       (propertize " " 'face 'variable-pitch)
       (all-the-icons-octicon "pencil" :v-adjust -0.05
                              :face 'mode-line-warn-face)
       ;; (propertize (format " (w:%s,c:%s)" words chars)
       ;;             'face `(:height 0.9))
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
                     ;; NOTE: Not terribly reliable
                     (- length (-elem-index this-oc iedit-occurrences-overlays))
                   "-"))
               length))
     'face 'mode-line-meta-face)))

;; multiple-cursors (mc/)
(defun *multiple-cursors ()
  "Show multiple-cursors indicator in mode-line."
  (if (and ; FIXME: (mc/fake-cursor-p OVERLAY)
       (> (mc/num-cursors) 1)) ; (if 'mc/fake-cursor-p ...)
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
  (when (and org-tree-slide-mode (org-tree-slide--active-p))
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
  "Show `major-mode' process info."
  (when (stringp mode-line-process)
    (propertize
     (concat
      (all-the-icons-faicon "circle-o" :v-adjust -0.05)
      mode-line-process)
     'face 'mode-line-warn-face
     'help-echo "buffer-process")))

;; notifications
;; IRC
(defun *erc ()
  "Show ERC info."
  ;; TODO:
  )

(defun *company-lighter ()
  "Show company-mode lighter from `company-lighter'."
  (if (and (boundp company-mode) company-mode (consp company-backend))
      (propertize
       (company--group-lighter
        (nth company-selection company-candidates) company-lighter-base)
       'face 'mode-line-data-face)))

(defun *org-clock ()
  "Show org-clock info."
  (when (and (active) org-clock-idle-timer)
    ;; get [0:05] from `org-clock-get-clock-string'
    (propertize
     (concat
      (all-the-icons-octicon "clock" :v-adjust 0.05)
      (format " %s"
              (org-minutes-to-clocksum-string (org-clock-get-clocked-time))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-modeline ()
  "My custom mode-line."
  `(:eval
    (let* ((meta (concat
                  (*emacsclient)
                  (*macro-recording)
                  (*selection-info)
                  (*anzu)
                  (*iedit)
                  (*multiple-cursors)
                  ;; (*evil-substitute)
                  (*input-method)
                  (*company-lighter)
                  ;; (*erc)
                  ))
           (lhs (list
                 (*current)
                 (if (= (length meta) 0) " %I " meta)
                 " "
                 (*buffer-info)
                 (*buffer-name)
                 ;; (*buffer-encoding)
                 (*linum-info)
                 ;; (*wc-mode)
                 (*pdf-tools-page-position)
                 (*org-clock)
                 (*org-tree-slide)
                 (*process)
                 ))
           (rhs (list
                 ;; NOTE: the `mid' `format-mode-line' meet first `nil' will
                 ;; count as 0. This will cause `rhs' to 0. So make sure the
                 ;; first fragment is not `nil'. So I use " " empty string with
                 ;; 1 length.
                 " "
                 (*flycheck)
                 (*build-status)
                 (*vc)
                 ;; (*buffer-project)
                 (*projectile)
                 ;; (*perspeen)
                 (*major-mode)
                 (*env)
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


;;; smart mode-line colors depend on background color.
;;; get current background color: light/dark?
(cl-case (alist-get 'background-mode (frame-parameters))
  ('light
   (set-face-attribute 'mode-line nil
                       :inherit nil
                       :height 1.0
                       )
   (set-face-attribute 'mode-line-inactive nil
                       :inherit nil
                       :height 1.0
                       )
   )
  
  ('dark
   ;; sci-fi dark style
   ;;
   ;; (set-face-attribute 'mode-line nil
   ;;                     :inverse-video nil
   ;;                     :foreground "white" :background "#004A5D"
   ;;                     :box '(:color "cyan" :line-width 1 :style nil)
   ;;                     :family "DejaVu Sans Mono"
   ;;                     )
   ;; (set-face-attribute 'mode-line-inactive nil
   ;;                     :inverse-video nil
   ;;                     :foreground "#444444" :background "black" ; :background "#242424"
   ;;                     :family "DejaVu Sans Mono"
   ;;                     :box '(:color "slate blue" :line-width -1 :style nil)
   ;;                     )

   ;; darker style
   ;;
   ;; (set-face-attribute 'mode-line nil
   ;;                     :inverse-video nil
   ;;                     :foreground "white"
   ;;                     :background "#484848"
   ;;                     :box '(:color "dark gray" :line-width 1 :style nil)
   ;;                     ;; :box '(:color "slate blue" :line-width 1 :style nil)
   ;;                     :family "DejaVu Sans Mono"
   ;;                     )
   ;; (set-face-attribute 'mode-line-inactive nil
   ;;                     :inverse-video nil
   ;;                     :foreground "dim gray"
   ;;                     :background (color-darken-name (face-background 'default) 3)
   ;;                     :family "DejaVu Sans Mono"
   ;;                     :box '(:color "dark slate gray" :line-width 1 :style nil)
   ;;                     )
   ))


(provide 'init-my-emacs-mode-line)

;;; init-my-emacs-mode-line.el ends here
