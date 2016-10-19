;;; init-my-tool-calculator.el --- init Tool Calculator
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Calc ]

(autoload 'calc "calc :: a calculator" t)

(setq calc-complex-format 'i            ; complex number style: x + yi.
      )

(unless (boundp 'my-calculator-map)
  (define-prefix-command 'my-calculator-map))
(define-key my-tools-prefix (kbd "C") 'my-calculator-map)

(define-key my-calculator-map (kbd "c") 'calc)
(define-key my-calculator-map (kbd "q") 'quick-calc) ; 'calc-keypad

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

(define-key my-calculator-map (kbd "x") 'mini-calc)


(provide 'init-my-tool-calculator)

;;; init-my-tool-calculator.el ends here
