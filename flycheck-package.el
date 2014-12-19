;;; flycheck-package.el --- Flycheck checker for elisp package metadata  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: lisp
;; Version: DEV
;; Package-Requires: ((cl-lib "0.5") (flycheck "0.22-cvs1") (emacs "24"))

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

;; Provides feedback via flycheck about issues with the package metadata
;; of a file, e.g. the package dependencies it requires.

;; To enable, use something like this:

;;    (eval-after-load 'flycheck
;;      '(flycheck-package-setup))

;; Checks will currently be enabled only if a "Package-Requires:" header
;; is present in the file.

;;; Code:

(require 'flycheck)
(require 'pcase) ; `pcase-dolist' is not autoloaded
(require 'package)

(flycheck-define-generic-checker 'emacs-lisp-package
  "A checker for \"Package-Requires\" headers."
  :start #'flycheck-package--start
  :modes '(emacs-lisp-mode))

;; Disclaimer: this is currently very hacky and will be cleaned up as & when it grows in scope.
(defun flycheck-package--start (checker callback)
  "Flycheck start function for checking metadata used by package.el."
  (let (errors)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (let ((case-fold-search t))
              (re-search-forward "^;+ *Package-Requires *: *\\(.*?\\) *$" nil t))
        (match-string 1)
        ;; Behold this horrible code. This is why monads, folks.
        (let* ((line-no (line-number-at-pos))
               (deps (match-string 1)))
          (condition-case err
              (pcase-let ((`(,parsed-deps . ,parse-end-pos) (read-from-string deps)))
                ;; Check for () wrapping entire dependency list
                (unless (eq parse-end-pos (length deps))
                  (push (list line-no 0 'error (format "More than one expression provided.")) errors))
                (let (valid-deps)
                  ;; Check for well-formed dependency entries
                  (dolist (entry parsed-deps)
                    (pcase entry
                      ((and `(,package-name ,package-version)
                            (guard (symbolp package-name))
                            (guard (stringp package-version)))
                       (if (ignore-errors (version-to-list package-version))
                           (push (cons package-name (version-to-list package-version)) valid-deps)
                         (push (list line-no 0 'error (format "%S is not a valid version string: see `version-to-string'." package-version)) errors)))
                      (_
                       (push (list line-no 0 'error (format "Expected (package-name \"version-num\"), but found %S." entry)) errors))))

                  (pcase-dolist (`(,package-name . ,package-version) valid-deps)
                    (unless (or (eq 'emacs package-name)
                                (assq package-name package-archive-contents))
                      (push (list line-no 0 'error (format "Package %S is not installable." package-name)) errors))
                    (unless (version-list-< package-version (list 19001201 1))
                      (push (list line-no 0 'warning (format "Use a non-snapshot version number for dependency on \"%S\" if possible." package-name)) errors)))
                  (when (save-excursion
                          (goto-char (point-min))
                          (re-search-forward ".*-\\*\\- +lexical-binding: +t" (line-end-position) nil))
                    (unless (assq 'emacs valid-deps)
                      (push (list 0 0 'warning (format "You should depend on (emacs \"24\") if you need lexical-binding.")) errors)))))
            (error
             (push (list line-no 0 'error (format "Couldn't parse \"Package-Requires\" header: %s" (error-message-string err))) errors)))
          (condition-case err
              (package-buffer-info)
            (error
             ;; Try fixing up the Version header before complaining
             (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
               (with-temp-buffer
                 (insert contents)
                 (flycheck-package--update-or-insert-version "0")
                 (condition-case err
                     (progn
                       (package-buffer-info)
                       (push (list 0 0 'warning "Missing a valid \"Version:\" header.") errors))
                   (error
                    (push (list 0 0 'error (format "package.el cannot parse this buffer: %s" (error-message-string err))) errors)))))))))
      (funcall callback 'finished
               (mapcar (lambda (e) (apply #'flycheck-error-new-at (append e (list :checker checker)))) errors)))))


(defun flycheck-package--update-or-insert-version (version)
  "Ensure current buffer has a \"Version: VERSION\" header."
  (goto-char (point-min))
  (if (let ((case-fold-search t)) (re-search-forward "^;* *Version *: *" nil t))
      (move-beginning-of-line nil)
    (forward-line))
  (insert (format ";; Version: %s" version))
  (newline))


;;;###autoload
(defun flycheck-package-setup ()
  "Setup flycheck-package.
Add `flycheck-emacs-lisp-package' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'emacs-lisp-package))


(provide 'flycheck-package)
;;; flycheck-package.el ends here
