;;; init-my-prog-tags-cscope.el --- init for cscope etc
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(defun cscope-build (dir)
  "My custom function to execute shell command: $ cscope -bR."
  (interactive "Dcscope build database directory: ")
  (let* ((dir (expand-file-name dir))
         (buffer-name (format "*cscope-build-%s" dir))
         (cscope-buffer (get-buffer-create buffer-name))
         process)
    (with-current-buffer cscope-buffer
      (if (get-buffer-process buffer-name)
          (kill-process (get-buffer-process buffer-name)))
      (setq default-directory dir)
      (setq process (start-file-process buffer-name buffer-name
                                        "cscope" "-bR"))
      (set-process-query-on-exit-flag process nil)
      (accept-process-output process 3)
      (if (looking-at "TODO: REGEXP about cscope build error")
          (progn
            (when cscope-buffer (kill-buffer cscope-buffer))
            (message "cscope build database failed"))
        (progn
          (message "cscope: database build %s : OK" dir))
        ))
    cscope-buffer))


;;; [ cscope ] -- This is an interface from GNUemacs to Joe Steffen's "cscope" C browser.

;; Usage:
;;
;; - $ cscope -bR :: normal usage command.
;; - $ cscope -b -R -q -k ::
;; - `cscope-minor-mode' :: enable minor mode will enable keybindings.

(require 'cscope)

;; (setq cscope-command-args)
(setq cscope-option-do-not-update-database t
      ;; cscope-option-include-directories ; -I
      ;; cscope-option-disable-compression
      cscope-option-kernel-mode nil ; -k
      cscope-option-use-inverted-index t ; -q
      ;; cscope-option-other
      )

;;; need to execute `cscope-minor-mode'.
;; (setq cscope-bindings-2deep nil
;;       cscope-bindings-3deep t
;;       cscope-blurb nil
;;       )



;;; a possibly handy hack:
;; (defun my-find-tag(&optional prefix)
;;   "union of `find-tag' alternatives. decides upon major-mode"
;;   (interactive "P")
;;   (if (and (boundp 'cscope-minor-mode)
;;          cscope-minor-mode)
;;       (progn
;;         (ring-insert find-tag-marker-ring (point-marker))
;;         (call-interactively
;;          (if prefix
;;              'cscope-find-this-symbol
;;            'cscope-find-global-definition-no-prompting
;;            )))
;;     (call-interactively 'find-tag)))
;;
;; (substitute-key-definition 'find-tag 'my-find-tag global-map)


;;; [ xcscope ] -- interface of cscope.

;; (require 'xcscope)


;;; [ ascope ] -- another interface of cscope.

;;; ascope is an improvement over xcscope that runs all queries through a single
;;; cscope process, instead of starting a new process and reloading the database
;;; for each query.

;; Usage:
;;
;; - `ascope-init' :: load the cscope database.
;;   This command must be issue prior to issue any other command below,
;;   the directory feed to this command must be the directory include
;;   the cscope.out file.
;; - `ascope-find-global-definition'
;; - `ascope-find-this-symbol'
;; - `ascope-find-this-text-string'
;; - `ascope-find-functions-calling-this-function'
;; - `ascope-find-called-functions'
;; - `ascope-find-files-including-file'
;; - `ascope-all-symbol-assignments'
;; - `ascope-clear-overlay-arrow'
;; - `ascope-pop-mark'
;;
;; run next commands in the search result buffer (*Result)
;;
;; - [n] :: `ascope-next-symbol'
;; - [p] :: `ascope-prev-symbol'
;; - [Enter] :: `ascope-select-entry-other-window-delete-window'

;; (require 'ascope)

;; TODO:
;; (define-key ascope-list-entry-mode-map (kbd "q")
;;   '(lambda ()
;;      (kill-buffer "*Result*")))


;;; [ bscope ]


;;; [ rscope ] -- It is born to challenge its cousins : xscope, ascope, bscope

;;; rscope is a new implementation taking its roots from ascope, thus running a
;;; single cscope process for each cscope database. It’s a bit more versatile
;;; than ascope because it copes with multiple cscope databases (and hence
;;; spawns one cscope process per database once).

;; - where is this variable used?
;; - what is the value of this preprocessor symbol?
;; - where is this function in the source files?
;; - what functions call this function?
;; - what functions are called by this function?
;; - where does the message "out of space" come from?
;; - where is this source file in the directory structure?
;; - what files include this header file?

;;; Usage:
;;
;; - [C-c s] :: prefix.

(require 'rscope)

(setq rscope-keymap-prefix (kbd "C-c l t c"))

(setq rscope-allow-arrow-overlays t
      rscope-overlay-arrow-string "=>"
      ;; rscope-separator-line "-------------------------------------------------------------------------------\n"
      rscope-hierarchies-shorten-filename t)

;; (set-face-attribute 'rscope-function-face nil
;;                     :foreground "green")
;; (set-face-attribute 'rscope-line-face nil
;;                     :foreground "orange")
;; (set-face-attribute 'rscope-line-number-face nil
;;                     :foreground "red")


;;; [ helm-cscope ] -- cscope with Helm interface.

;;; Usage:
;;
;; interactive functions
;;
;; - `helm-cscope-find-symbol'
;; - `helm-cscope-find-global-definition'
;; - `helm-cscope-find-called-function'
;; - `helm-cscope-find-calling-this-funtcion'
;; - `helm-cscope-select' (uses all of above sources)

(require 'helm-cscope)

(dolist (hook '(prog-mode-hook
                c-mode-hook
                c++-mode-hook
                ))
  (add-hook hook 'helm-cscope-mode))

;; ;;; set key-bindings
;; ;; 1. better then 2.
;; (add-hook 'helm-cscope-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd (concat my-prog-help-document-map " M-t")) 'helm-cscope-find-symbol)))


;; ;; 2.
;; (eval-after-load "helm-cscope"
;;   '(progn
;;      ;; use "C-h d l" as prefix. so use "C-h d" prefix map.
;;      (define-key helm-cscope-mode-map (kbd (concat my-prog-help-document-map "M-t"))
;;        'helm-cscope-find-symbol)
;;      (define-key helm-cscope-mode-map (kbd (concat my-prog-help-document-map "M-r"))
;;        'helm-cscope-find-global-definition)
;;      (define-key helm-cscope-mode-map (kbd (concat my-prog-help-document-map "M-g M-c"))
;;        'helm-cscope-find-called-function)
;;      (define-key helm-cscope-mode-map (kbd (concat my-prog-help-document-map "M-g M-p"))
;;        'helm-cscope-find-calling-this-funtcion)
;;      (define-key helm-cscope-mode-map (kbd (concat my-prog-help-document-map "M-s"))
;;        'helm-cscope-select)
;;      ))



(unless (boundp 'my-prog-lookup-tags-cscope-map)
  (define-prefix-command 'my-prog-lookup-tags-cscope-map)
  (define-key my-prog-lookup-tags-map (kbd "c") 'my-prog-lookup-tags-cscope-map))

(define-key my-prog-lookup-tags-cscope-map (kbd "b") 'cscope-build)

(cond
 ;; [ rscope ]
 ((featurep 'rscope)
  (define-key my-prog-lookup-tags-cscope-map (kbd "s") 'rscope-find-this-symbol)
  (define-key my-prog-lookup-tags-cscope-map (kbd "=") 'rscope-all-symbol-assignments)
  (define-key my-prog-lookup-tags-cscope-map (kbd "d") 'rscope-find-global-definition)
  (define-key my-prog-lookup-tags-cscope-map (kbd "c") 'rscope-find-functions-calling-this-function)
  (define-key my-prog-lookup-tags-cscope-map (kbd "C") 'rscope-find-called-functions)
  (define-key my-prog-lookup-tags-cscope-map (kbd "t") 'rscope-find-this-text-string)
  (define-key my-prog-lookup-tags-cscope-map (kbd "i") 'rscope-find-files-including-file)
  (define-key my-prog-lookup-tags-cscope-map (kbd "h") 'rscope-find-calling-hierarchy)
  
  (define-key my-prog-lookup-tags-cscope-map (kbd "n") 'cscope-history-backward-line-current-result)
  (define-key my-prog-lookup-tags-cscope-map (kbd "N") 'cscope-history-forward-file-current-result)
  )
 
 ;; [ helm-cscope ]
 ((and (featurep 'helm) (featurep 'helm-cscope))
  (define-key my-prog-lookup-tags-cscope-map (kbd "s") 'helm-cscope-select)
  (define-key my-prog-lookup-tags-cscope-map (kbd "c") 'helm-cscope-find-symbol)
  (define-key my-prog-lookup-tags-cscope-map (kbd "q") 'helm-cscope-find-called-function)
  (define-key my-prog-lookup-tags-cscope-map (kbd "r") 'helm-cscope-find-calling-this-funtcion)
  
  (define-key my-prog-lookup-tags-cscope-map (kbd "n") 'cscope-history-backward-line-current-result)
  (define-key my-prog-lookup-tags-cscope-map (kbd "N") 'cscope-history-forward-file-current-result)
  )

 ;; [ cscope ]
 (t
  ;; FIXME: (define-key my-prog-lookup-tags-cscope-map (kbd "s") 'cscope-select-entry-one-window)
  (define-key my-prog-lookup-tags-cscope-map (kbd "c") 'cscope-find-this-symbol)
  
  (define-key my-prog-lookup-tags-cscope-map (kbd "n") 'cscope-history-backward-line-current-result)
  (define-key my-prog-lookup-tags-cscope-map (kbd "N") 'cscope-history-forward-file-current-result)
  )
 )


(provide 'init-my-prog-tags-cscope)

;;; init-my-prog-tags-cscope.el ends here
