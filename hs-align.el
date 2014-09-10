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

(defvar hs-align-operator-list '("=" "::" "--" "|"))

(defun hs-align ()
  (interactive)
  (mapc 'hs-align-search hs-align-operator-list))

(defun hs-align-search (operator)
  "Align operators in current block."
  (setq op-list nil)
  (save-excursion
    (let (beg end (count 0) line-count (maxcol 0))
      (setq line-count (line-number-at-pos))
      (forward-line 0)
      (re-search-backward (concat "^\\([^" operator
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
	  (setq op-list (cons (list posi leng space col) op-list)
		maxcol (max maxcol (- col space -1)))))
      (hs-align-insert op-list maxcol))))

(defun hs-align-insert (oplist maxcol)
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
