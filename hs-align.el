;;; hs-align --- align operators in haskell

;; Copyright (C) 2014 Florian Knupfer

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: Florian Knupfer
;; email: (rot13 "sxahcsre@tznvy.pbz")

;;; Commentary:

;; Bugs and feature requests can be send via email.

;;; Code:

(require 'haskell-mode)

(defvar hs-align-operator-list '(("=" 10) ("::" 6)
				 ("--" 6) (">>=" 4)
				 ("<-" 3) ("->" 3) ("=>" 3)
				 ("==" 2) ("/=" 2)
				 ("<=" 2) ("<" 2)
				 (">=" 2) (">" 2)
				 ("|" 2) ("." 2)
				 ("/" 1) ("*" 1)
				 ("+" 1) ("-" 1)
				 ("(" 1) (")" 1)
				 ("[" 1) ("]" 1)))


(define-minor-mode hs-align-mode
  "Auto align operators and other keywords.

The alignment is in the source file, so take care.  It checks
lines consecutive lines, which contain point and an operator from
`hs-align-operator-list' and aligns them if the maximal column
difference is lower than the second argument in
`hs-align-operator-list."
  :init-value nil
  :lighter " >>="
  :global nil
  (if hs-align-mode (add-hook 'after-change-functions 'hs-align-block nil t)
    (remove-hook 'after-change-functions 'hs-align-block t)))

(defun hs-align-block (a b c)
  "Map the operators to the search.

Vars A B and C are required by `after-change-functions' but
thrown away."
  (mapc 'hs-align-search hs-align-operator-list)
  (mapc 'hs-align-search (reverse hs-align-operator-list)))

(defun hs-align-search (enum-operator)
  "Align operators in current block.

Receive a ENUM-OPERATOR list and split it in operators and
distances."
  (let ((operator (regexp-quote (car enum-operator)))
	(operator-non-esc (car enum-operator))
	(dist (cadr enum-operator))
	(oplist nil))
    (save-excursion
      (let (beg end (count 0) line-count (maxcol 0) (mincol 72))
	(setq line-count (line-number-at-pos))
	(forward-line 0)
	(re-search-backward (concat "^\\([^" operator-non-esc
				    "]\\|[^ ]" operator
				    "\\|" operator
				    "[^ ]\\)*$") nil t)
	(setq line-count (line-number-at-pos))
	(while (and (re-search-forward
		     (concat "^.*?\\( +\\)"
			     operator " .+$") nil t)
		    (<= -1 (- line-count (setq line-count
					       (line-number-at-pos)))))
	  (let ((posi (match-end 1))
		(leng (- (match-end 0) (match-beginning 0)))
		(space (- (match-end 1) (match-beginning 1)))
		(col (- (match-end 1) (match-beginning 0))))
	    (setq oplist (cons (list posi leng space col) oplist)
		  maxcol (max maxcol (- col space -1))
		  mincol (min mincol (- col space -1)))))
	(when (and oplist (<=  (- maxcol mincol) dist))
	  (hs-align-insert oplist maxcol))))))

(defun hs-align-insert (oplist maxcol)
  "Insert or remove whitespace.

Retrieve from OPLIST pos of operator and compare it to MAXCOL,
which represents the leftmost operator in this block."
  (while oplist
    (let ((elem (pop oplist)))
      (save-excursion
	(goto-char (nth 0 elem))
	(if (and (> 72 (nth 1 elem)) (> maxcol (nth 3 elem)))
	    (insert (make-string (min (- 72 (nth 1 elem))
				      (- maxcol (nth 3 elem))) ?\s))
	  (if (< maxcol (nth 3 elem))
	      (delete-region (- (nth 0 elem) (- (nth 3 elem) maxcol))
			     (nth 0 elem))
	    (when (and (<= 72 (nth 1 elem)) (< 1 (nth 2 elem)))
	      (delete-region (- (nth 0 elem) (min (1- (nth 2 elem))
						  (- (nth 1 elem) 72)))
			     (nth 0 elem)))))))))

(provide 'hs-align)

;;; hs-align.el ends here
