;;; latex-math-symbol.el --- LaTeX math symbol decoder

;; Copyright (C) 1996,1997 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1996/7/1
;; Version:
;;    $Id: latex-math-symbol.el,v 1.3 1997/01/14 11:30:27 morioka Exp morioka $
;; Keywords: LaTeX, math, mule

;; This file is part of MU (Message Utilities).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; - How to install
;;	bytecompile this file and copy it to the apropriate directory.
;; - How to use
;;	If you use tm, please put following to your ~/.emacs:
;;	  (autoload 'latex-math-decode-buffer "latex-math-symbol" nil t)
;;	  (add-hook 'mime-viewer/plain-text-preview-hook
;;		    'latex-math-decode-buffer)
;;   Of course, it may be available for other hooks to filter messages.

;;; Code:

(defvar latex-math-symbol-table-alist
  '(("\\alpha"		. ",Fa(B")
    ("\\beta"		. ",Fb(B")
    ("\\gamma"		. ",Fc(B")("\\Gamma"	. "$B&#(B")
    ("\\delta"		. ",Fd(B")("\\Delta"	. "$B&$(B")
    ("\\epsilon"	. ",Fe(B")("\\varepsilon"	. "$B&E(B")
    ("\\zeta"		. ",Ff(B")
    ("\\eta"		. ",Fg(B")
    ("\\theta"		. ",Fh(B")("\\Theta"	. "$B&((B")
    ("\\iota"		. ",Fi(B")
    ("\\kappa"		. ",Fj(B")
    ("\\lambda"		. ",Fk(B")("\\Lambda"	. "$B&+(B")
    ("\\mu"		. ",Fl(B")
    ("\\nu"		. ",Fm(B")
    ("\\xi"		. ",Fn(B")("\\Xi"		. "$B&.(B")
    ("\\pi"		. ",Fp(B")("\\Pi"		. "$B&0(B")
    ("\\rho"		. ",Fq(B")
    ("\\sigma"		. ",Fs(B")("\\Sigma"	. "$B&2(B")
    ("\\varsigma"	. ",Fr(B")
    ("\\tau"		. ",Ft(B")
    ("\\upsilon"	. ",Fu(B")("\\Upsilon"	. "$B&4(B")
    ("\\phi"		. "$B&U(B")("\\Phi"		. "$B&5(B")
    ("\\varphi"		. ",Fv(B")
    ("\\chi"		. ",Fw(B")
    ("\\psi"		. ",Fx(B")("\\Psi"		. "$B&7(B")
    ("\\omega"		. ",Fy(B")("\\Omega"	. "$B&8(B")
    
    ("\\{"		. "$B!P(B")("\\}"		. "$B!Q(B")
    ("\\langle\\!\\langle" . "$B!T(B")("\\rangle\\!\\rangle" . "$B!U(B")
    ("\\langle"		. "$B!R(B")("\\rangle"	. "$B!S(B")
    
    ("\\cdots"		. "$B!D(B")
    
    ("\\ln"		. "$(G"L(B")
    ("\\log"		. "$(G"K(B")
    
    ("\\pm"		. "$B!^(B")
    ("\\cdot"		. "$B!&(B")
    ("\\times"		. "$B!_(B")("\\ast"		. "$B!v(B")
    ("\\star"		. "$B!z(B")
    ("\\bullet"		. "$B!&(B")
    ("\\div"		. "$B!`(B")
    ("\\cap"		. "$B"A(B")("\\cup"		. "$B"@(B")
    ("\\lhd"		. "$(C"7(B")("\\rhd"		. "$(C"9(B")
    ("\\bigcirc"	. "$B"~(B")
    ("\\vee"		. "$B"K(B")("\\lor"		. "$B"K(B")
    ("\\wedge"		. "$B"J(B")("\\land"	. "$B"J(B")
    ("\\oplus"		. "$(G"S(B")
    ("\\odot"		. "$(G"T(B")
    ("\\dagger"		. "$B"w(B")("\\ddagger"	. "$B"x(B")
    
    ("\\leq"		. "$(C!B(B")("\\geq"		. "$(C!C(B")
    ("\\le"		. "$(C!B(B")("\\ge"		. "$(C!C(B")
    ("\\ll"		. "$B"c(B")("\\gg"		. "$B"d(B")
    ("\\subseteq"	. "$B"<(B")("\\supseteq"	. "$B"=(B")
    ("\\subset"		. "$B">(B")("\\supset"	. "$B"?(B")
    ("\\in"		. "$B":(B")
    ("\\ni"		. "$B";(B")("\\owns"	. "$B";(B")
    ("\\frown"		. "$B"^(B")
    ("\\mid"		. "$B!C(B")("\\parallel"	. "$B!B(B")
    ("\\sim"		. "$B!A(B")
    ("\\equiv"		. "$B"a(B")
    ("\\approx"		. "$A!V(B")
    ("\\not="		. "$B!b(B")
    ("\\neq"		. "$B!b(B")("\\ne"		. "$B!b(B")
    ("\\perp"		. "$B"](B")
    
    ("\\triangleup"	. "$B"$(B")
    ("\\forall"		. "$B"O(B")
    
    ("\\hbar"		. ",C1(B")("\\imath"	. ",C9(B")
    ("\\ell"		. "$(C'$(B")
    ("\\partial"	. "$B"_(B")
    ("\\infty"		. "$B!g(B")
    ("\\smallint"	. "$B"i(B")
    ("\\P"		. "$B"y(B")
    ("\\prime"		. "$B!l(B")
    ("\\nabla"		. "$B"`(B")
    ("\\top"		. "$(D0#(B")("\\bot"		. "$(D0"(B")
    ("\\vert"		. "$B!C(B")("\\Vert"	. "$B!B(B")
    ("\\angle"		. "$B"\(B")
    ("\\triangle"	. "$B"$(B")
    ("\\backslash"	. "$B!@(B")
    ("\\S"		. "$B!x(B")
    ("\\forall"		. "$B"O(B")
    ("\\exists"		. "$B"P(B")
    ("\\neg"		. "$B"L(B")("\\lnot"	. "$B"L(B")
    ("\\flat"		. "$B"u(B")("\\sharp"	. "$B"t(B")
    ("\\clubsuit"	. "$(C"@(B")
    ("\\diamondsuit"	. "$B!~(B")
    ("\\heartsuit"	. "$(C"=(B")
    ("\\spadesuit"	. "$(C"<(B")
    
    ("\\leftarrow"	. "$B"+(B")("\\rightarrow"	. "$B"*(B")
    ("\\gets"		. "$B"+(B")("\\to"		. "$B"*(B")
    
    ("^1"		. ",A9(B")("^{1}"		. ",A9(B")
    ("^2"		. ",A2(B")("^{2}"		. ",A2(B")
    ("^3"		. ",A3(B")("^{3}"		. ",A3(B")
    ("^4"		. "$(C)y(B")("^{4}"		. "$(C)y(B")
    ("^n"		. "$(C)z(B")("^{n}"		. "$(C)z(B")
    ("_1"		. "$(C){(B")("_{1}"		. "$(C){(B")
    ("_2"		. "$(C)|(B")("_{2}"		. "$(C)|(B")
    ("_3"		. "$(C)}(B")("_{3}"		. "$(C)}(B")
    ("_4"		. "$(C)~(B")("_{4}"		. "$(C)~(B")
    ))

;;;###autoload
(defun latex-math-decode-region (beg end)
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (let ((rest latex-math-symbol-table-alist)
	  (case-fold-search nil)
	  cell)
      (while rest
	(setq cell (car rest))
	(goto-char beg)
	(while (re-search-forward
		(concat "\\("
			(regexp-quote (car cell))
			"\\)\\([^a-zA-Z]\\|$\\)")
		nil t)
	  (delete-region (match-beginning 1)(match-end 1))
	  (goto-char (match-beginning 0))
	  (insert (cdr cell))
	  )
	(setq rest (cdr rest))
	))))

;;;###autoload
(defun latex-math-decode-buffer ()
  (interactive)
  (latex-math-decode-region (point-min)(point-max))
  )


;;; @ end
;;;

(provide 'latex-math-symbol)

;;; latex-math-symbol.el ends here
