;;; init-my-prog-tags-cscope.el --- init for cscope etc
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ cscope ] -- This is an interface from GNUemacs to Joe Steffen's "cscope" C browser.

;; Usage:
;; $ cscope -bR
;; - `cscope-minor-mode' ::

(require 'cscope)

(setq cscope-bindings-2deep nil
      cscope-bindings-3deep t
      cscope-blurb nil
      )



;;; [ xcscope ] -- interface of cscope.

;; (require 'xcscope)


;;; [ ascope ] -- another interface of cscope.

;;; ascope is an improvement over xcscope that runs all queries through a single
;;; cscope process, instead of starting a new process and reloading the database
;;; for each query.

;; Usage:
;;load this script using (require 'ascope.el) in you .emacs
;; M-x ascope-init load the cscope database. This command must be issue prior to issue any other command below, the directory feed to this command must be the directory include the cscope.out file
;; M-x ascope-find-global-definition
;; M-x ascope-find-this-symbol
;; M-x ascope-find-this-text-string
;; M-x ascope-find-functions-calling-this-function
;; M-x ascope-find-called-functions
;; M-x ascope-find-files-including-file
;; M-x ascope-all-symbol-assignments
;; M-x ascope-clear-overlay-arrow
;; M-x ascope-pop-mark
;;
;;run next commands in the search result buffer (*Result)
;;ascope-next-symbol this command is bind to key "n"
;;ascope-prev-symbol this command is bind to key "p"
;;ascope-select-entry-other-window-delete-window this command is bind to key "enter"

(require 'ascope)


;;; [ rscope ]

;;; rscope is a new implementation taking its roots from ascope, thus running a
;;; single cscope process for each cscope database. It’s a bit more versatile
;;; than ascope because it copes with multiple cscope databases (and hence
;;; spawns one cscope process per database once).



;;; [ helm-cscope ] -- cscope with Helm interface.

;;; Usage:
;;
;; interactive functions
;;
;;     - helm-cscope-find-symbol
;;     - helm-cscope-find-global-definition
;;     - helm-cscope-find-called-function
;;     - helm-cscope-find-calling-this-funtcion
;;     - helm-cscope-select (uses all of above sources)

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

(define-key my-prog-lookup-tags-map (kbd "c") 'helm-cscope-find-symbol)





(provide 'init-my-prog-tags-cscope)

;;; init-my-prog-tags-cscope.el ends here
