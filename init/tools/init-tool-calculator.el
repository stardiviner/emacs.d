;;; init-tool-calculator.el --- init Tool Calculator
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Calc ]

(autoload 'calc "calc :: a calculator" t)

(setq calc-complex-format 'i            ; complex number style: x + yi.
      )

(unless (boundp 'calculator-prefix)
  (define-prefix-command 'calculator-prefix))
(define-key tools-prefix (kbd "C") 'calculator-prefix)

(define-key calculator-prefix (kbd "c") 'calc)
(define-key calculator-prefix (kbd "q") 'quick-calc) ; 'calc-keypad

;; usefull mini calculator
(defun mini-calc (expr &optional arg)
  "Calculate expression

If ARG is given, then insert the result to current-buffer"
  (interactive
   (list (read-from-minibuffer "Enter expression: ")
         current-prefix-arg))

  (let ((result (calc-eval expr)))
    (if arg
        (insert result)
      (message (format "Result: [%s] = %s" expr result)))))

(define-key calculator-prefix (kbd "x") 'mini-calc)


(provide 'init-tool-calculator)

;;; init-tool-calculator.el ends here
