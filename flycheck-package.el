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

(eval-when-compile (require 'pcase))    ; `pcase-dolist' is not autoloaded
(eval-when-compile (require 'cl-lib))
(require 'flycheck)
(require 'package)

(cl-defstruct (flycheck-package--context
               (:constructor nil)
               (:constructor flycheck-package--create-context (checker))
               (:copier nil)
               (:predicate nil))
  (checker nil :read-only t)
  (error-list nil)
  (pass-results (make-hash-table :test #'eq) :read-only t))

(defvar flycheck-package--registered-passes '())

(defun flycheck-package--call-pass (context pass)
  (let ((pass-results (flycheck-package--context-pass-results context)))
    (pcase-let ((`(,code . ,actual-result)
                 (or (gethash pass pass-results)
                     (puthash pass
                              (condition-case err
                                  (cons 'ok (funcall pass context))
                                (error (cons 'error err)))
                              pass-results))))
      (if (eq 'error code)
          (signal (car actual-result) (cdr actual-result))
        actual-result))))

(defun flycheck-package--start (checker callback)
  (let ((context (flycheck-package--create-context checker)))
    (dolist (pass flycheck-package--registered-passes)
      (condition-case-unless-debug nil
          (flycheck-package--call-pass context pass)
        (error)))
    (funcall callback
             'finished
             (mapcar (lambda (x)
                       (apply #'flycheck-error-new-at x))
                     (flycheck-package--context-error-list context)))))

(defun flycheck-package--error (context line column level message)
  (push (list line column level message
              :checker (flycheck-package--context-checker context))
        (flycheck-package--context-error-list context)))

(defun flycheck-package--register-pass (check)
  (cl-pushnew check flycheck-package--registered-passes :test #'eq))

(eval-and-compile
  (defun flycheck-package--expand-pass-name (name)
    (intern (concat "flycheck-package--pass-" (symbol-name name)))))

(defmacro flycheck-package--define-pass (name arglist &rest body)
  (declare (indent defun) (debug t) (doc-string 3))
  (let ((real-name (flycheck-package--expand-pass-name name)))
    `(prog1 (defun ,real-name ,arglist
              ,@body)
       (flycheck-package--register-pass #',real-name))))

(defmacro flycheck-package--require-pass (binding pass context &rest body)
  (declare (indent 3) (debug t))
  `(pcase-let ((,binding
                (flycheck-package--call-pass
                 ,context
                 #',(flycheck-package--expand-pass-name pass))))
     ,@body))

(flycheck-package--define-pass get-dependency-list (_context)
  (save-excursion
    (save-restriction
      (widen)
      (if (flycheck-package--goto-header "Package-Requires")
          (cons (line-number-at-pos) (match-string 1))
        (signal 'error '("No Package-Requires found"))))))

(flycheck-package--define-pass parse-dependency-list (context)
  (flycheck-package--require-pass
      `(,line-no . ,deps) get-dependency-list context
    (condition-case err
        (pcase-let ((`(,parsed-deps . ,parse-end-pos) (read-from-string deps)))
          (unless (= parse-end-pos (length deps))
            (flycheck-package--error
             context line-no 0 'error
             "More than one expression provided."))
          (cons line-no parsed-deps))
      (error
       (flycheck-package--error
        context line-no 0 'error
        (format "Couldn't parse \"Package-Requires\" header: %s" (error-message-string err)))
       ;; Rethrow, because there's no point in trying to recover.
       (signal (car err) (cdr err))))))

(flycheck-package--define-pass get-well-formed-dependencies (context)
  (flycheck-package--require-pass
      `(,line-no . ,parsed-deps) parse-dependency-list context
    (let ((valid-deps '()))
      (dolist (entry parsed-deps)
        (pcase entry
          ((and `(,package-name ,package-version)
                (guard (symbolp package-name))
                (guard (stringp package-version)))
           (if (ignore-errors (version-to-list package-version))
               (push (cons package-name
                           (version-to-list package-version))
                     valid-deps)
             (flycheck-package--error
              context line-no 0 'error
              (format "%S is not a valid version string: see `version-to-string'."
                      package-version))))
          (_
           (flycheck-package--error
            context line-no 0 'error
            (format "Expected (package-name \"version-num\"), but found %S." entry)))))
      (cons line-no valid-deps))))

(flycheck-package--define-pass packages-installable (context)
  (flycheck-package--require-pass
      `(,_ . ,valid-deps) get-well-formed-dependencies context
    (pcase-dolist (`(,package-name . ,_) valid-deps)
      (unless (or (eq 'emacs package-name)
                  (assq package-name package-archive-contents))
        (pcase-let ((`(,line-no ,offset)
                     (flycheck-package--position-of-dependency package-name)))
          (flycheck-package--error
           context line-no offset 'error
           (format "Package %S is not installable." package-name)))))))

(flycheck-package--define-pass deps-use-non-snapshot-version (context)
  (flycheck-package--require-pass
      `(,_ . ,valid-deps) get-well-formed-dependencies context
    (pcase-dolist (`(,package-name . ,package-version) valid-deps)
      (unless (version-list-< package-version (list 19001201 1))
        (pcase-let ((`(,line-no ,offset)
                     (flycheck-package--position-of-dependency package-name)))
          (flycheck-package--error
           context line-no offset 'warning
           (format "Use a non-snapshot version number for dependency on \"%S\" if possible."
                   package-name)))))))

(flycheck-package--define-pass deps-do-not-use-zero-versions (context)
  (flycheck-package--require-pass
      `(,_ . ,valid-deps) get-well-formed-dependencies context
    (pcase-dolist (`(,package-name . ,package-version) valid-deps)
      (when (equal package-version '(0))
        (pcase-let ((`(,line-no ,offset)
                     (flycheck-package--position-of-dependency package-name)))
          (flycheck-package--error
           context line-no offset 'warning
           (format "Use a properly versioned dependency on \"%S\" if possible."
                   package-name)))))))

(flycheck-package--define-pass lexical-binding-requires-emacs-24 (context)
  (when (save-excursion
          (goto-char (point-min))
          (re-search-forward ".*-\\*\\- +lexical-binding: +t" (line-end-position) t))
    (flycheck-package--require-pass
        `(,line-no . ,valid-deps) get-well-formed-dependencies context
      (unless (assq 'emacs valid-deps)
        (flycheck-package--error
         context line-no 0 'warning
         "You should depend on (emacs \"24\") if you need lexical-binding.")))))

(flycheck-package--define-pass do-not-depend-on-cl-lib-1.0 (context)
  (flycheck-package--require-pass
      `(,_ . ,valid-deps) get-well-formed-dependencies context
    (let ((cl-lib-version (cdr (assq 'cl-lib valid-deps))))
      (when (and cl-lib-version
                 (version-list-<= '(1) cl-lib-version))
        (pcase-let ((`(,line-no ,offset)
                     (flycheck-package--position-of-dependency 'cl-lib)))
          (flycheck-package--error
           context line-no offset 'error
           (format "Depend on the latest 0.x version of cl-lib rather than on version \"%S\".
Alternatively, depend on Emacs 24.3, which introduced cl-lib 1.0."
                   cl-lib-version)))))))

(flycheck-package--define-pass package-el-can-parse-buffer (context)
  (flycheck-package--require-pass _ get-dependency-list context
    (condition-case nil
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
                 (flycheck-package--error
                  context 0 0 'warning
                  "Missing a valid \"Version:\" header."))
             (error
              (flycheck-package--error
               context 0 0 'error
               (format "package.el cannot parse this buffer: %s" (error-message-string err)))))))))))

(defun flycheck-package--position-of-dependency (package-name)
  (save-excursion
    (when (flycheck-package--goto-header "Package-Requires")
      (move-beginning-of-line nil)
      (let ((line-start (point))
            (line-no (line-number-at-pos))
            (pattern (format "( *%s\\(?:)\\|[^[:alnum:]_\\-].*?)\\)" package-name)))
        (when (re-search-forward pattern (line-end-position) t)
          (message "%S => %S" pattern (match-data))
          (list line-no (- (1+ (match-beginning 0)) line-start)))))))

(defun flycheck-package--goto-header (header-name)
  "Move to the first occurrence of HEADER-NAME in the file.
If the return value is non-nil, then point will be at the end of
the file, and the first match group will contain the value of the
header with any leading or trailing whitespace removed."
  (let ((initial-point (point)))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^;+ *" (regexp-quote header-name) " *: *\\(.*?\\) *$") nil t)
          (point)
        (goto-char initial-point)
        nil))))

(defun flycheck-package--update-or-insert-version (version)
  "Ensure current buffer has a \"Version: VERSION\" header."
  (if (flycheck-package--goto-header "Version")
      (move-beginning-of-line nil)
    (forward-line))
  (insert (format ";; Version: %s" version))
  (newline))

(flycheck-define-generic-checker 'emacs-lisp-package
  "A checker for \"Package-Requires\" headers."
  :start #'flycheck-package--start
  :modes '(emacs-lisp-mode))

;;;###autoload
(defun flycheck-package-setup ()
  "Setup flycheck-package.
Add `flycheck-emacs-lisp-package' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'emacs-lisp-package))

(provide 'flycheck-package)
;;; flycheck-package.el ends here
