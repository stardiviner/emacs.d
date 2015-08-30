;;; init-my-emacs-debug.el --- init for Emacs debug

;;; Commentary:

;;; Tips:
;; - $ emacs --debug-init
;; - $ emacs -Q
;; - $ emacs -q


;;; Code:

;;; [ debug ] -- Emacs built-in debugger.

;;; Usage:
;;
;;   M-x debug-on-entry FUNCTION
;;   M-x cancel-debug-on-entry &optional FUNCTION
;;   M-x toggle-debug-on-quit
;;   (debug &rest DEBUGGER-ARGS)

;; [C-h i g (elisp) Debugging RET]

;; - debug-on-error t
;; - debug-on-quit t
;; - debug-on-signal nil
;; - debug-on-next-call nil
;; - debug-on-event
;; - debug-on-message nil ; REGEXP

;; If your init file sets debug-on-error, the effect may not last past the end
;; of loading the init file. (This is an undesirable byproduct of the code that
;; implements the `--debug-init' command line option.) The best way to make the
;; init file set debug-on-error permanently is with after-init-hook, like this:
;
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (setq debug-on-error t)))

;;; Debug: Trace
(setq stack-trace-on-error t)


;; Sometimes you want to find out where a particular error, warning or just
;; plain annoying message in Messages is coming from.
;; This piece of advice allows you to see the function call sequence that
;; resulted in each message in the Messages buffer:

;; (defadvice message (before who-said-that activate)
;;   "Find out who said that thing. and say so."
;;   (let ((trace nil) (n 1) (frame nil))
;;     (while (setq frame (backtrace-frame n))
;;       (setq n     (1+ n)
;;             trace (cons (cadr frame) trace)) )
;;     (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 0)))
;;     (ad-set-args 1 (cons trace (ad-get-args 1)))
;;     ))

;; ;;; To deactivate this, call
;; (ad-disable-advice 'message 'before 'who-said-that)
;; (ad-update 'message)

;; ;;; Similarly, to get timestamps:
;; (defadvice message (before when-was-that activate)
;;   "Add timestamps to `message' output."
;;   (ad-set-arg 0 (concat (format-time-string "[%Y-%m-%d %T %Z] ")
;;                         (ad-get-arg 0)) ))

;;; And to deactivate this, call
;; (ad-disable-advice 'message 'before 'when-was-that)
;; (ad-update 'message)



;;; [ Checkpoints ]

;; Since, this is one huge file, it is often hard to debug where a particular
;; error has occurred, and therefore, I need some visual clue of some type,
;; a.k.a. checkpoints. The following functions, together, help me with that. I
;; can, simply, make a call to the my/checkpoint function, in order to
;; echo something inside my *Messages* buffer, and immediately, know nearby
;; location of where Emacs has stopped loading this configuration. Not to
;; mention, these checkpoints, further, help me by acting as indirect comments.

;; subtract two time entities
(defun my/time-subtract-millis (b a)
  "Function that can subtract time string A from time string B."
  (* 1000.0 (float-time (time-subtract b a))))

;; convenient function to measure load-time since initialization
(defun my/load-time()
  "Return total load-time from the initialization."
  (my/time-subtract-millis (current-time) before-init-time))

;; function to display which section is being loaded..
(defun my/checkpoint (msg)
  "Echo MSG to *Messages*, thereby, making it act as a checkpoint."
  (if debug-on-error (message "- At =%.2fms=, I %s.." (my/load-time) msg)))

;; an example of above
(my/checkpoint "initialized benchmarking")


;;; [ Edebug ] -- Edebug is a source level debugger.

;; FIXME:
;; (eval-after-load "edebug"
;;   '(progn
;;      (define-key edebug-mode-map (kbd "C-c C-d") nil)))

(require 'edebug)
(require 'edebug-x)

(setq edebug-global-prefix (kbd "C-c d"))

(unless (boundp 'my-prog-debug-map)
  (define-prefix-command 'my-prog-debug-map))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c d") 'my-prog-debug-map)
            (define-key my-prog-debug-map (kbd "C-e") 'edebug-mode)
            (define-key my-prog-debug-map (kbd "f") 'edebug-defun)
            (define-key my-prog-debug-map (kbd "e") 'debug-on-entry)
            ))

(set-face-attribute 'hi-edebug-x-stop nil
                    :foreground nil
                    :background "plum1")
(set-face-attribute 'hi-edebug-x-debug-line nil
                    :foreground nil
                    :background "light green")

(defun edebug-clear-global-break-condition ()
  "Clear `edebug-global-break-condition'."
  (interactive)
  (setq edebug-global-break-condition nil))


;;; [ edebug-x ]

;;; Extensions to Edebug to make it a little nicer to work with. Provides
;;; highlighting for breakpoints, instrumented functions and current line of
;;; debugger. Also provides a couple of commands to list current breakpoints and
;;; instrumented functions.

;;; Usage:
;;
;; --------------- [C-x SPC] ---------------------------
;;
;;; This package provides the following functions:
;;
;; edebug-x-modify-breakpoint-wrapper     - toggle breakpoints in Elisp buffer, [C-x SPC]
;;                                          When called with a prefix argument a conditional breakpoint is set
;; edebug-x-show-breakpoints              - show a tabulated list of all breakpoints, [C-c C-x b]
;; edebug-x-show-instrumented             - show a tabulated list of instrumented functions, [C-c C-x i]
;; edebug-x-show-data                     - show both the breakpoints and instrumented functions buffer, [C-c C-x s]
;;
;;; From the tabulated list buffer the following commands are available:
;;
;; edebug-x-kill-breakpoint               - bound to [K], clear breakpoint
;; edebug-x-visit-breakpoint              - bound to [RET], visit breakpoint location
;;
;;; The instrumented functions buffer has these commands:
;;
;; edebug-x-evaluate-function             - bound to [E], evaluate function, clearing breakpoints within it
;; edebug-x-find-function bound to        - bound to [RET], jump to function

(require 'edebug-x)

;; (setq edebug-x-stop-point-overlay nil)


;;; [ Benchmarking ]

;; Moreover, since, I am a quantified-geek, I love to measure various
;; things. Why not measure time taken by our Emacs configuration, as well?

;; This section, also, enables me to measure the time taken by various features
;; in requiring them, as well as total time taken by the Emacs to load this
;; configuration. When Emacs load this configuration, it displays which features
;; were require‘d, and how much time that took. This is, especially, useful for
;; debugging which module is making our Emacs start-up, so slow.

;; function to display how much time a particular feature took to require..
(defun my/require-time-message(package time)
  (if debug-on-error ( message
                       "- At =%.2fms=, I required a feature: =%s=, which took me =%0.2fms=."
                       (my/load-time) package time)))

(defvar feature-required-time nil "Require time for a specific feature.")

(defvar my/require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require
    (around build-require-times (feature &optional filename noerror) activate)
  "Note in `my/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features) debug-on-error)
        (setq feature-required-time
              (my/time-subtract-millis (current-time) require-start-time))
        (my/require-time-message feature feature-required-time)
        (add-to-list 'my/require-times
                     (cons feature
                           (my/time-subtract-millis (current-time)
                                                    require-start-time))
                     t)))))


;;; [ profiler ]

;;; Usage:
;;
;; - `profiler-start' ::
;; - `profiler-stop' ::
;; - `profiler-report' ::

(unless (boundp 'my-emacs-profiler-prefix)
  (define-prefix-command 'my-emacs-profiler-prefix))
(define-key my-prog-debug-map (kbd "p") 'my-emacs-profiler-prefix)

(define-key my-emacs-profiler-prefix (kbd "p") 'profiler-start)
(define-key my-emacs-profiler-prefix (kbd "s") 'profiler-stop)
(define-key my-emacs-profiler-prefix (kbd "r") 'profiler-report)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c d") 'my-prog-debug-map)
            (define-key my-prog-debug-map (kbd "p") 'profiler-start)
            (define-key my-prog-debug-map (kbd "s") 'profiler-stop)
            (define-key my-prog-debug-map (kbd "r") 'profiler-report)
            ))


;;; [ bug-hunter ] -- Hunt down errors in elisp files.

;;; The Bug Hunter is an Emacs library that finds the source of an error or
;;; unexpected behavior inside an elisp configuration file (typically init.el or
;;; .emacs).

;;; Usage:
;;
;; - [M-x bug-hunter-init-file RET RET]
;;     If your Emacs init file signals an error during startup, but you don’t know why, simply issue.
;;
;;
;; For example, let’s say there’s something in your init file that’s loading the
;; cl library, and you don’t want that. You know you’re not loading it yourself,
;; but how can you figure out which external package is responsible for this
;; outrage?
;;
;;   [M-x bug-hunter-init-file RET (featurep 'cl) RET]
;;
;; Finally, you can also use `bug-hunter-file' to hunt in other files.

(require 'bug-hunter)





(provide 'init-my-emacs-debug)

;;; init-my-emacs-debug.el ends here
