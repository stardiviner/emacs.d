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
      (propertize "▌░"
                  'face '(:foreground "yellow"))
    ""))

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
    (if (derived-mode-p 'prog-mode)
        (all-the-icons-faicon "file-code-o" :v-adjust -0.05)
      (all-the-icons-faicon "file-o" :v-adjust -0.05))
    (propertize " " 'face 'variable-pitch)
    (propertize (buffer-name)
                'face (if (active) 'mode-line-buffer-path-face 'mode-line-inactive))
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
          (when (buffer-modified-p)
            (all-the-icons-faicon "floppy-o"
                                  :face 'mode-line-warn-face
                                  :v-adjust -0.1)))
        (when (and buffer-file-name (not (file-exists-p buffer-file-name)))
          (all-the-icons-octicon "circle-slash"
                                 :face 'mode-line-info-face
                                 :v-adjust -0.05))
        (when (file-remote-p (buffer-file-name))
          (all-the-icons-faicon "download"
                                'face 'mode-line-warn-face
                                :v-adjust -0.1))
        " "))))

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
              'face '(:height 0.75))
  )

;;; major-mode
(defun *major-mode ()
  "The major mode, including process, environment and text-scale info."
  (propertize
   (concat
    (if (featurep 'major-mode-icons)
        (major-mode-icons-show)
      (if (featurep 'all-the-icons)
          (all-the-icons-icon-for-mode major-mode)
        (format-mode-line mode-name)))
    (if (stringp mode-line-process) mode-line-process)
    "  ")
   'face (if (active) 'mode-line-buffer-major-mode-face 'mode-line-inactive)))

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
          (all-the-icons-faicon "git"
                                :height 1.1 :v-adjust -0.05)))
       (propertize " " 'face 'variable-pitch)
       (cond ((memq state '(edited added))
              (if active (setq face 'mode-line-info-face))
              (all-the-icons-octicon "git-branch" :face face :height 1.1 :v-adjust -0.05))
             ((eq state 'needs-merge)
              (if active (setq face 'mode-line-warn-face))
              (all-the-icons-octicon "git-merge" :face face))
             ((eq state 'needs-update)
              (if active (setq face 'mode-line-warn-face))
              (all-the-icons-octicon "arrow-down" :face face))
             ((memq state '(removed conflict unregistered))
              (if active (setq face 'mode-line-urgent-face))
              (all-the-icons-octicon "alert" :face face))
             (t
              (if active (setq face 'mode-line))
              (all-the-icons-octicon "git-branch"
                                     :face face :height 1.1 :v-adjust -0.05)))
       " "
       (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                   'face (if active face))
       " "
       (propertize " " 'face 'variable-pitch)))))

;;; flycheck
(defvar-local mode-line--flycheck-err-cache nil "")
(defvar-local mode-line--flycheck-cache nil "")

(defun mode-line--flycheck-count (state)
  "Return flycheck information for the given error type STATE."
  (when (flycheck-has-current-errors-p state)
    (if (eq 'running flycheck-last-status-change)
        "?"
      (cdr-safe (assq state (flycheck-count-errors flycheck-current-errors))))))

(defun *flycheck ()
  "Persistent and cached flycheck indicators in the mode-line."
  (when (and (featurep 'flycheck) flycheck-mode)
    (if (or flycheck-current-errors
            (eq 'running flycheck-last-status-change))
        (or (and (or (eq mode-line--flycheck-err-cache mode-line--flycheck-cache)
                     (memq flycheck-last-status-change '(running not-checked)))
                 (if (eq flycheck-last-status-change 'running)
                     (concat " "
                             (all-the-icons-octicon
                              "ellipsis"
                              :face (if (active)
                                        'mode-line-info-face
                                      'mode-line-inactive)
                              :height 1.1
                              :v-adjust 0)
                             " ")
                   mode-line--flycheck-cache))
            (and (setq mode-line--flycheck-err-cache flycheck-current-errors)
                 (setq mode-line--flycheck-cache
                       (let ((fw (mode-line--flycheck-count 'warning))
                             (fe (mode-line--flycheck-count 'error)))
                         (concat
                          (if (or fe fw) " ")
                          (if fe
                              (concat
                               (all-the-icons-octicon "circle-slash"
                                                      :face 'mode-line-warn-face
                                                      :v-adjust 0.05)
                               (propertize " " 'face 'variable-pitch)
                               (propertize (format "%d" fe)
                                           'face (if (active)
                                                     'mode-line-warn-face
                                                   'mode-line-inactive))
                               " "))
                          (if fw
                              (concat
                               (all-the-icons-octicon "alert"
                                                      :face (if (active)
                                                                'mode-line-urgent-face
                                                              'mode-line-inactive)
                                                      :v-adjust 0.05)
                               (propertize " " 'face 'variable-pitch)
                               (propertize (format "%d" fw)
                                           'face (if (active)
                                                     'mode-line-urgent-face
                                                   'mode-line-inactive))
                               " "))
                          (if (or fe fw)
                              " "
                            (when (active)
                              (all-the-icons-octicon "check" :v-adjust -0.06))))))))
      (concat
       " "
       (all-the-icons-octicon "check"
                              :face (if (active) 'mode-line-info-face 'mode-line-inactive)
                              :v-adjust -0.06)
       " "))))

;; selection info
(defun *selection-info ()
  "Information about the current selection.

Such as how many characters and lines are selected, or the NxM
dimensions of a block selection."
  (when (and (active) (evil-visual-state-p))
    (concat
     " "
     (propertize
      (let ((reg-beg (region-beginning))
            (reg-end (region-end))
            (evil (eq 'visual evil-state)))
        (let ((lines (count-lines reg-beg (min (1+ reg-end) (point-max))))
              (chars (- (1+ reg-end) reg-beg))
              (cols (1+ (abs (- (evil-column reg-end)
                                (evil-column reg-beg))))))
          (cond
           ;; rectangle selection
           ((or (bound-and-true-p rectangle-mark-mode)
                (and evil (eq 'block evil-visual-selection)))
            (format " %dx%dB " lines (if evil cols (1- cols))))
           ;; line selection
           ((or (> lines 1) (eq 'line evil-visual-selection))
            (if (and (eq evil-state 'visual) (eq evil-this-type 'line))
                (format " %dL " lines)
              (format " %dC %dL " chars lines)))
           (t (format " %dC " (if evil chars (1- chars)))))))
      'face 'mode-line-meta-face))))

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
  ;; FIXME: (mc/fake-cursor-p OVERLAY)
  (if (and (mc/fake-cursor-p)
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
  (when (and org-tree-slide--active-p (active))
    (propertize (format "[%s]" org-tree-slide--slide-number)
                'face 'mode-line-info-face)))

;; wc-mode (word count) `wc-format-modeline-string', `wc-mode-update'.
(defun *wc-mode ()
  "Show wc-mode word count."
  (if (and (featurep 'wc-mode) wc-mode (active))
      (propertize (wc-format-modeline-string " WC:[%tw]")
                  'face 'mode-line-info-face)))

;; mmm-mode


;; process: inferior,
(defun *process ()
  "Show `major-mode' process info."
  (when (stringp mode-line-process)
    (propertize
     (concat " ◌ " mode-line-process)
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
       'face 'mode-line-warn-face)))

(defun *org-clock ()
  "Show org-clock info."
  (when (and (active) org-clock-idle-timer)
    ;; get [0:05] from `org-clock-get-clock-string'
    (propertize
     (concat
      (all-the-icons-octicon "clock"
                             :v-adjust 0.1)
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
                  (*anzu)
                  (*iedit)
                  ;; (*multiple-cursors)
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
                 ;; (*wc-mode)
                 (*linum-info)
                 (*process)
                 (*org-clock)
                 ;; (*org-tree-slide)
                 ))
           (rhs (list
                 (*flycheck)
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
   )
  ('dark
   ;; sci-fi dark style
   ;;
   ;; (set-face-attribute 'mode-line nil
   ;;                     :inverse-video nil
   ;;                     :foreground "white" :background "#004A5D"
   ;;                     :box '(:color "cyan" :line-width 1 :style nil)
   ;;                     :family "DejaVu Sans Mono"
   ;;                     :height 90
   ;;                     )
   ;; (set-face-attribute 'mode-line-inactive nil
   ;;                     :inverse-video nil
   ;;                     :foreground "#444444" :background "black" ; :background "#242424"
   ;;                     :family "DejaVu Sans Mono"
   ;;                     :box '(:color "slate blue" :line-width -1 :style nil)
   ;;                     :height 90
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
   ;;                     :height 100
   ;;                     )
   ;; (set-face-attribute 'mode-line-inactive nil
   ;;                     :inverse-video nil
   ;;                     :foreground "dim gray"
   ;;                     :background (color-darken-name (face-background 'default) 3)
   ;;                     :family "DejaVu Sans Mono"
   ;;                     :box '(:color "dark slate gray" :line-width 1 :style nil)
   ;;                     :height 100
   ;;                     )
   ))


(provide 'init-my-emacs-mode-line)

;;; init-my-emacs-mode-line.el ends here
