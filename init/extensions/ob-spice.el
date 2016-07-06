;;; ob-spice.el --- org-babel functions for spice evaluation

;; Author: Tiago Oliveira Weber
;; Version: 0.1
;; Homepage: http://tiagoweber.github.io

;; Org-Babel support for evaluating spice script.
;; Inspired by Ian Yang's org-export-blocks-format-plantuml (http://www.emacswiki.org/emacs/org-export-blocks-format-plantuml.el)

;;; Requirements:
;; ngspice

;;; Code:
(require 'ob)

(add-to-list 'org-babel-tangle-lang-exts '("spice" . "cir"))

(defun org-babel-expand-body:spice (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* (
	 (vars (mapcar #'cdr (org-babel-get-header params :var)))
	 )
    ;; replace any variable names preceded by '$' with the actual
    ;; value of the variable
    (mapc (lambda (pair)
	    (setq body (replace-regexp-in-string
			(format "\\$%s" (car pair)) (cdr pair) body)))
	  vars)
    )
  body
  )

(defun org-babel-execute:spice (body params)
  "Execute a block of Spice code with org-babel."
  (let ((body (org-babel-expand-body:spice body params))
	(vars (mapcar #'cdr (org-babel-get-header params :var)))	
	)    
    (org-babel-eval "ngspice -b " body)
    (setq textfile (concat (cdar vars) ".txt"))
    ;; produce results    
    (concat
     (if (file-readable-p textfile)
	 (get-string-from-file textfile))
     '"#+ATTR_HTML: :width 600px"
     (concat "\n [[file:./" (cdar vars) ".png]]")
     )
    )
  )

(provide 'ob-spice)
;;; ob-spice.el ends here
