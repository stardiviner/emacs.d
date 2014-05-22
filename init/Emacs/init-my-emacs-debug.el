;;; init-my-emacs-debug.el --- init for Emacs debug

;;; Commentary:

;;; Tips:
;; - $ emacs --debug-init
;; - $ emacs -Q
;; - $ emacs -q


;;; Code:

(setq debug-on-error t
      debug-on-quit nil
      debug-on-signal nil)

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
;; can, simply, make a call to the stardiviner/checkpoint function, in order to
;; echo something inside my *Messages* buffer, and immediately, know nearby
;; location of where Emacs has stopped loading this configuration. Not to
;; mention, these checkpoints, further, help me by acting as indirect comments.

;; subtract two time entities
(defun stardiviner/time-subtract-millis (b a)
  "Function that can subtract time string A from time string B."
  (* 1000.0 (float-time (time-subtract b a))))

;; convenient function to measure load-time since initialization
(defun stardiviner/load-time()
  "Return total load-time from the initialization."
  (stardiviner/time-subtract-millis (current-time) before-init-time))

;; function to display which section is being loaded..
(defun stardiviner/checkpoint (msg)
  "Echo MSG to *Messages*, thereby, making it act as a checkpoint."
  (if debug-on-error (message "- At =%.2fms=, I %s.." (stardiviner/load-time) msg)))

;; an example of above
(stardiviner/checkpoint "initialized benchmarking")


;;; [ Edebug ] -- Edebug is a source level debugger.

;;; Extensions to Edebug to make it a little nicer to work with. Provides
;;; highlighting for breakpoints, instrumented functions and current line of
;;; debugger. Also provides a couple of commands to list current breakpoints and
;;; instrumented functions.

(require 'edebug-x)

(setq edebug-x-stop-point-overlay t)


;;; [ Benchmarking ]

;; Moreover, since, I am a quantified-geek, I love to measure various
;; things. Why not measure time taken by our Emacs configuration, as well?

;; This section, also, enables me to measure the time taken by various features
;; in requiring them, as well as total time taken by the Emacs to load this
;; configuration. When Emacs load this configuration, it displays which features
;; were require‘d, and how much time that took. This is, especially, useful for
;; debugging which module is making our Emacs start-up, so slow.

;; function to display how much time a particular feature took to require..
(defun stardiviner/require-time-message(package time)
  (if debug-on-error ( message
                       "- At =%.2fms=, I required a feature: =%s=, which took me =%0.2fms=."
                       (stardiviner/load-time) package time)))

(defvar feature-required-time nil "Require time for a specific feature.")

(defvar stardiviner/require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require
    (around build-require-times (feature &optional filename noerror) activate)
  "Note in `stardiviner/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features) debug-on-error)
        (setq feature-required-time
              (stardiviner/time-subtract-millis (current-time) require-start-time))
        (stardiviner/require-time-message feature feature-required-time)
        (add-to-list 'stardiviner/require-times
                     (cons feature
                           (stardiviner/time-subtract-millis (current-time)
                                                          require-start-time))
                     t)))))




(provide 'init-my-emacs-debug)

;;; init-my-emacs-debug.el ends here
