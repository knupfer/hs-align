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

(defun hs-align ()
  "Align operators in current block."
  (interactive)
  (setq op-list nil)
  (save-excursion
    (let (beg end (count 0) line-count)
      (setq line-count (line-number-at-pos))
      (forward-line 0)
      (re-search-backward "^[^ ]" nil t)
      (setq line-count (line-number-at-pos))
      (while (and (re-search-forward "^.*[^ ]\\( +\\)= .+$" nil t)
		  (<= -1 (- line-count (setq line-count
					     (line-number-at-pos)))))
	(let ((leng (- (match-end 0) (match-beginning 0)))
	      (space (- (match-end 1) (match-beginning 1)))
	      (pos (match-end 1)))
	  (setq op-list (cons (list leng space pos) op-list)))))))

(provide 'hs-align)

;;; hs-align.el ends here
