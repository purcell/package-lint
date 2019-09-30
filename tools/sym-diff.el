;;; sym-diff.el --- Diff dumped elisp symbols produced by sym-dump.el -*- lexical-binding: t -*-

;; Copyright (C) 2019  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/package-lint
;; Keywords: lisp
;; Version: 0
;; Package-Requires: ((seq "0.5") (emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Diff dumped elisp symbols produced by sym-dump.el

;;; Code:

(require 'seq)

(defun sym-diff-hashdiff (v1 v2)
  (let ((in-v2 (make-hash-table)))
    (dolist (x v2)
      (puthash x t in-v2))
    (seq-filter (lambda (e) (not (gethash e in-v2))) v1)))

(defun sym-diff (dump1 dump2 type)
  (let ((v1 (cdr (assoc type dump1)))
        (v2 (cdr (assoc type dump2))))
    (sort (sym-diff-hashdiff v1 v2)
          (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun sym-diff-load-dump (f)
  (with-temp-buffer
    (insert-file-contents f)
    (goto-char 0)
    (read (current-buffer))))

(defun sym-diff-versions (v1 v2)
  `((functions . ((removed . ,(sym-diff v1 v2 'functions))
                  (added . ,(sym-diff v2 v1 'functions))))
    (variables . ((removed . ,(sym-diff v1 v2 'variables))
                  (added . ,(sym-diff v2 v1 'variables))))
    (features . ((removed . ,(sym-diff v1 v2 'features))
                 (added . ,(sym-diff v2 v1 'features))))
    ))

(defun sym-diff-dumps (f1 f2)
  (let ((v1 (sym-diff-load-dump f1))
        (v2 (sym-diff-load-dump f2)))
    (sym-diff-versions v1 v2)))

(provide 'sym-diff)

;;; sym-diff.el ends here
