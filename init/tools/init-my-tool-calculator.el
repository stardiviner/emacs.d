;;; init-my-tool-calculator.el --- init Tool Calculator
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

;;; [ Calc ]

;;; Usage:
;;;
;;; - [M-x calc] --run calc.
;;; - [C-x * c] -- start calculator.
;;;     - press ' to start Algebra input. e.g. ' sqrt(2+3) <RET>
;;; Standard user interface
;;; Quick mode interface
;;; Keypad mode interface
;;; - [C-x * k] -- keypad mode.
;;;     - click on those keypad buttons.
;;; Embedded mode
;;
;; - [' 1+1 RET] :: Algebraic style.
;; - [1 RET 3 +] :: RPN style.
;; - [C-x * 0] :: calc-reset.
;;
;; GET HELP
;; - get help -> [C-h i g (calc)]
;; - in Calculator window.
;;   - [C-h m] :: mode help.
;;   - [h i] :: go to info.
;;   - [h ?] :: get short help
;;   - [h ? ?] :: more help.
;;   - [h s] :: summary of help.
;;   - [h t] :: tutorial of help.
;; - [C-x *] :: prefix
;; - [C-x * *] ::
;; ELISP> (calc-eval "1+2")
;;
;; - Plot
;;   - [g p] :: graph-plot.

(autoload 'calc "calc :: a calculator" t)

(setq calc-complex-format 'i            ; complex number style: x + yi.
      )

;; TODO: set Calc default to normal calculator mode.


(unless (boundp 'my-calculator-map)
  (define-prefix-command 'my-calculator-map)
  (define-key my-tools-prefix (kbd "x") 'my-calculator-map))

(if (featurep 'helm)
    (define-key my-calculator-map (kbd "x") 'helm-calcul-expression)
  (define-key my-calculator-map (kbd "x") 'calc) ; 'calc-keypad
  )


(provide 'init-my-tool-calculator)

;;; init-my-tool-calculator.el ends here
