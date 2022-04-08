;;; sym-dump.el --- Dump data about symbols and features available in this Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2019  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/package-lint
;; Keywords: lisp
;; Version: 0

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

;; This is all written in a weird way in order to minimise the code loaded
;; during its execution.

;; Also, this code must work all the way back to Emacs 23.4.

;;; Code:

(defun sym-dump-sort-symbols (syms)
  (sort syms (lambda (a b) (string< (symbol-name a) (symbol-name b)))))

(defun sym-dump-filter-atoms (pred)
  (let (result)
    (mapatoms
     (lambda (f)
       (when (and (funcall pred f)
                  (not (keywordp f))
                  (not (string-prefix-p "sym-dump-" (symbol-name f))))
         (push f result))))
    (sym-dump-sort-symbols result)))

(defun sym-dump-defined-functions ()
  (sym-dump-filter-atoms 'fboundp))

(defun sym-dump-defined-vars ()
  (sym-dump-filter-atoms 'boundp))

(defun sym-dump-defined-faces ()
  (sym-dump-filter-atoms 'facep))

(defun sym-dump-libraries (path)
  (let (libs)
    (dolist (dir path)
      (when (file-accessible-directory-p dir)
        (dolist (f (directory-files dir t "\\.elc\\'" t))
          (let ((lib (intern (file-name-sans-extension (file-name-nondirectory f)))))
            ;; Skip files that aren't loadable libraries, e.g. blessmail, edt-mapper, dunnet
            ;; Additionally, loading secrets and tramp-gvfs causes a hard exit if no dbus support
            (unless (memq lib '(blessmail edt-mapper dunnet secrets tramp-gvfs gnus-registry))
              (push lib libs))))))
    (sym-dump-sort-symbols libs)))

(defun sym-dump-loaded ()
  `((variables . ,(sym-dump-defined-vars))
    (functions . ,(sym-dump-defined-functions))
    (features . ,features)
    (faces . ,(sym-dump-defined-faces))))

(defun sym-dump-go-crazy ()
  (dolist (lib (sym-dump-libraries load-path))
    (message "Loading %s" lib)
    (with-demoted-errors (require lib nil t)))
  (message "Loaded all")
  (let (print-level print-length) ; avoid truncation
    (pp (sym-dump-loaded))))

(sym-dump-go-crazy)

;;; sym-dump.el ends here

