;;; ob-powershell.el --- org-babel functions for powershell evaluation

;; Authors: Chris Bilson
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;;; Commentary:

;; Org-Babel support for evaluating powershell source code.


;;; Code:
(require 'ob)
(require 'ob-core)
(eval-when-compile (require 'cl))

(defcustom org-babel-powershell-command "powershell"
  "The command to run PowerShell src block code."
  :type 'string
  :safe #'stringp
  :group 'org)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("poweshell" . "ps1"))

(defvar org-babel-default-header-args:powershell '())

(defun org-babel-expand-body:powershell (body params)
  body)

(defun org-babel-execute:powershell (body params)
  "Execute a block of Powershell code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((in-file (org-babel-temp-file "powershell-" ".ps1"))
         (cmdline (or (cdr (assq :cmdline params))
                      "-NoLogo -NonInteractive"))
         (cmd (or (cdr (assq :cmd params))
                  org-babel-powershell-command)))
    (with-temp-file in-file
      (insert (org-babel-expand-body:powershell body params)))
    (message "cmd: %s" cmd)
    (message "cmdline: %s" cmdline)
    (message "in-file: %s" in-file)
    (message "body: %s" (org-babel-expand-body:powershell body params))
    (org-babel-eval
     (concat cmd " " cmdline
             " -File " (org-babel-process-file-name in-file))
     "")))

;; TODO: I think I *can* support sessions in powershell and really want to...
(defun org-babel-prep-session:powershell (session params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (error "Sessions are not supported for Powershell"))

(defun org-babel-variable-assignments:powershell (params)
  "Return list of powershell statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (org-babel-powershell--var-to-powershell (cdr pair) (car pair)))
   (mapcar #'cdr (org-babel--get-vars params))))

;; helper functions

(defvar org-babel-powershell--lvl 0)

(defun org-babel-powershell--var-to-powershell (var &optional varn)
  "Convert an elisp value to a powershell variable.
The elisp value, VAR, is converted to a string of powershell source code
specifying a var of the same value."
  (if varn
      (let ((org-babel-powershell--lvl 0) (lvar (listp var)) prefix)
        (concat "$" (symbol-name varn) "=" (when lvar "\n")
                (org-babel-powershell--var-to-powershell var)
                ";\n"))
    (let ((prefix (make-string (* 2 org-babel-powershell--lvl) ?\ )))
      (concat prefix
              (if (listp var)
                  (let ((org-babel-powershell--lvl (1+ org-babel-powershell--lvl)))
                    (concat "[\n"
                            (mapconcat #'org-babel-powershell--var-to-powershell var "")
                            prefix "]"))
                (format "${%s}" var))
              (unless (zerop org-babel-powershell--lvl) ",\n")))))

(defvar org-babel-powershell-buffers '(:default . nil))

(defun org-babel-powershell-initiate-session (&optional session params)
  "Return nil because sessions are not supported by powershell."
  nil)

(defvar org-babel-powershell-preface nil)

(defun org-babel-powershell-evaluate (session ibody &optional result-type result-params)
  "Pass BODY to the Powershell process in SESSION.
If RESULT-TYPE equals 'output then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals 'value then
return the value of the last statement in BODY, as elisp."
  (when session (error "Sessions are not supported for Powershell"))
  (let* ((body (concat org-babel-powershell-preface ibody))
         (out-file (org-babel-temp-file "powershell-"))
         (tmp-babel-file (org-babel-process-file-name
                          out-file 'noquote))
         (in-file (org-babel-temp-file "powershell-"))
         (command (format "%s -File '%s'" org-babel-powershell-command in-file)))

    (with-temp-file in-file
      (insert body))

    (message "body: %s" body)
    (message "in-file: %s" in-file)
    (message "out-file: %s" out-file)
    (message "command: %s" command)

    (let ((results
           (case result-type
             (output
              (with-temp-file out-file
                (insert
                 (org-babel-eval org-babel-powershell-command body))
                (buffer-string)))
             (value
              (message "evaliing now...")
              (org-babel-eval command body)))))
      (when results
        (org-babel-result-cond result-params
          (org-babel-eval-read-file out-file)
          (org-babel-import-elisp-from-file out-file '(16)))))))

(provide 'ob-powershell)
;;; ob-powershell.el ends here
