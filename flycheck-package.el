;;; flycheck-package.el --- A Flycheck checker for elisp package authors -*- lexical-binding: t -*-

;; Copyright (C) 2014  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: lisp
;; Version: 0
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
(require 'lisp-mnt)


;;; Machinery

(cl-defstruct (flypkg/context
               (:constructor nil)
               (:constructor flypkg/create-context (checker))
               (:copier nil)
               (:predicate nil))
  (checker nil :read-only t)
  (error-list nil)
  (pass-results (make-hash-table :test #'eq) :read-only t))

(defvar flypkg/registered-passes '())

(put 'flypkg/failed-pass 'error-conditions '(flypkg/failed-pass error))

(defun flypkg/call-pass (context pass)
  (let ((pass-results (flypkg/context-pass-results context)))
    (pcase-let ((`(,code . ,actual-result)
                 (or (gethash pass pass-results)
                     (puthash pass
                              (condition-case err
                                  (cons 'ok (funcall pass context))
                                (flypkg/failed-pass
                                 (cons 'error err)))
                              pass-results))))
      (if (eq 'error code)
          (signal (car actual-result) (cdr actual-result))
        actual-result))))

(defun flypkg/start (checker callback)
  "Flycheck checker start function."
  (funcall callback
           'finished
           (mapcar (lambda (x)
                     (apply #'flycheck-error-new-at x))
                   (condition-case err
                       (let ((context (flypkg/create-context checker)))
                         (dolist (pass flypkg/registered-passes)
                           (condition-case nil
                               (flypkg/call-pass context pass)
                             (flypkg/failed-pass)))
                         (flypkg/context-error-list context))
                     (error
                      (funcall callback 'errored (error-message-string err))
                      (signal (car err) (cdr err)))))))

(defun flypkg/error (context line column level message)
  (push (list line column level message :checker (flypkg/context-checker context))
        (flypkg/context-error-list context)))

(defun flypkg/register-pass (check)
  (add-to-list 'flypkg/registered-passes check))

(defmacro flypkg/define-pass (name arglist &rest body)
  (declare (indent defun) (debug t) (doc-string 3))
  (let* ((docstring (when (stringp (car body)) (car body)))
         (body (if docstring (cdr body) body)))
    `(prog1 (defun ,name ,arglist
              ,docstring
              (save-excursion
                (save-restriction
                  (save-match-data
                    (widen)
                    ,@body))))
       (flypkg/register-pass #',name))))


;;; Passes for each check

(flypkg/define-pass flypkg/get-dependency-list (_context)
  "Return position and contents of the \"Package-Requires\" header.
If no such header is present, fail the pass."
  (if (flypkg/goto-header "Package-Requires")
      (list (match-beginning 3) (line-number-at-pos) (match-string 3))
    (signal 'flypkg/failed-pass '("No Package-Requires found"))))

(flypkg/define-pass flypkg/parse-dependency-list (context)
  "Check that the \"Package-Requires\" header contains a single valid lisp expression."
  (pcase-let ((`(,position ,line-no ,deps)
               (flypkg/call-pass context #'flypkg/get-dependency-list)))
    (condition-case err
        (pcase-let ((`(,parsed-deps . ,parse-end-pos) (read-from-string deps)))
          (unless (= parse-end-pos (length deps))
            (flypkg/error
             context line-no 1 'error
             "More than one expression provided."))
          (list position line-no parsed-deps))
      (error
       (flypkg/error
        context line-no 1 'error
        (format "Couldn't parse \"Package-Requires\" header: %s" (error-message-string err)))
       (signal 'flypkg/failed-pass err)))))

(flypkg/define-pass flypkg/get-well-formed-dependencies (context)
  "Check that listed dependencies are in the format (name \"version\")."
  (pcase-let ((`(,position ,line-no ,parsed-deps)
               (flypkg/call-pass context #'flypkg/parse-dependency-list)))
    (let ((valid-deps '()))
      (dolist (entry parsed-deps)
        (pcase entry
          ((and `(,package-name ,package-version)
                (guard (symbolp package-name))
                (guard (stringp package-version)))
           ;; Find the column at which the dependency is declared so we can
           ;; properly report the position of errors.
           (let ((offset
                  (save-excursion
                    (goto-char position)
                    (let ((line-start (line-beginning-position))
                          (pattern
                           (format "( *\\(%s\\)\\(?:)\\|[^[:alnum:]_\\-].*?)\\)" package-name)))
                      (if (re-search-forward pattern (line-end-position) t)
                          (- (1+ (match-beginning 1)) line-start)
                        1)))))
             (if (ignore-errors (version-to-list package-version))
                 (push (list package-name
                             (version-to-list package-version)
                             offset)
                       valid-deps)
               (flypkg/error
                context line-no offset 'error
                (format "%S is not a valid version string: see `version-to-list'."
                        package-version)))))
          (_
           (flypkg/error
            context line-no 1 'error
            (format "Expected (package-name \"version-num\"), but found %S." entry)))))
      (cons line-no valid-deps))))

(flypkg/define-pass flypkg/packages-installable (context)
  "Check that every package listed in \"Package-Requires\" is available for installation."
  (pcase-let ((`(,line-no . ,valid-deps)
               (flypkg/call-pass context #'flypkg/get-well-formed-dependencies)))
    (pcase-dolist (`(,package-name ,package-version ,offset) valid-deps)
      (if (eq 'emacs package-name)
          (unless (version-list-<= (list 24) package-version)
            (flypkg/error
             context line-no offset 'error
             "You can only depend on Emacs version 24 or greater."))
        ;; Not 'emacs
        (let ((archive-entry (assq package-name package-archive-contents)))
          (if archive-entry
              (let ((best-version (flypkg/lowest-installable-version-of package-name)))
                (when (version-list-< best-version package-version)
                  (flypkg/error
                   context line-no offset 'warning
                   (format "Version dependency for %s appears too high: try %s" package-name
                           (package-version-join best-version)))))
            (flypkg/error
             context line-no offset 'error
             (format "Package %S is not installable." package-name))))))))

(flypkg/define-pass flypkg/deps-use-non-snapshot-version (context)
  "Warn about apparent dependencies on snapshot versions of packages."
  (pcase-let ((`(,line-no . ,valid-deps)
               (flypkg/call-pass context #'flypkg/get-well-formed-dependencies)))
    (pcase-dolist (`(,package-name ,package-version ,offset) valid-deps)
      (unless (version-list-< package-version (list 19001201 1))
        (flypkg/error
         context line-no offset 'warning
         (format "Use a non-snapshot version number for dependency on \"%S\" if possible."
                 package-name))))))

(flypkg/define-pass flypkg/deps-do-not-use-zero-versions (context)
  "Warn about sloppy dependencies on \"0\" versions of packages."
  (pcase-let ((`(,line-no . ,valid-deps)
               (flypkg/call-pass context #'flypkg/get-well-formed-dependencies)))
    (pcase-dolist (`(,package-name ,package-version ,offset) valid-deps)
      (when (equal package-version '(0))
        (flypkg/error
         context line-no offset 'warning
         (format "Use a properly versioned dependency on \"%S\" if possible."
                 package-name))))))

(flypkg/define-pass flypkg/lexical-binding-requires-emacs-24 (context)
  "Warn about lexical-binding without depending on Emacs 24."
  (goto-char (point-min))
  (when (cdr (assq 'lexical-binding (flypkg/get-header-line-file-local-variables)))
    (let* ((lexbind-line (line-number-at-pos))
           (lexbind-col (1+ (- (match-beginning 1) (line-beginning-position))))
           (valid-deps
            (cdr (flypkg/call-pass context #'flypkg/get-well-formed-dependencies))))
      (unless (assq 'emacs valid-deps)
        (flypkg/error
         context lexbind-line lexbind-col 'warning
         "You should depend on (emacs \"24\") if you need lexical-binding.")))))

(flypkg/define-pass flypkg/lexical-binding-must-be-in-first-line (context)
  "Check that any lexical-binding declaration is on the first line of the file."
  (cl-block return
    (let ((original-buffer (current-buffer)))
      (with-temp-buffer
        (let ((lexical-binding-found-at-end nil))
          (insert-buffer-substring-no-properties original-buffer)
          (condition-case err
              (cl-letf (((symbol-function #'hack-local-variables-apply) #'ignore)
                        ((symbol-function #'hack-local-variables-filter)
                         (lambda (variables _dir-name)
                           (setq file-local-variables-alist variables)))
                        ;; Silence any messages Emacs may want to share with the user.
                        ;; There's no user.
                        ((symbol-function #'display-warning) #'ignore)
                        ((symbol-function #'message) #'ignore))
                ;; HACK: this is an internal variable!
                ;; Unfortunately, Emacsen that have this variable also have
                ;; `hack-local-variables' that doesn't store `lexical-binding'
                ;; in `file-local-variables-alist'.
                (defvar hack-local-variables--warned-lexical)
                (let ((hack-local-variables--warned-lexical nil)
                      (enable-dir-local-variables nil)
                      (enable-local-variables t)
                      (local-enable-local-variables t))
                  (hack-local-variables)
                  (setq lexical-binding-found-at-end
                        hack-local-variables--warned-lexical)))
            (error
             (flypkg/error context 1 1 'error (error-message-string err))
             (cl-return-from return nil)))
          (when (or lexical-binding-found-at-end
                    ;; In case this is an Emacs from before `hack-local-variables'
                    ;; started to warn about `lexical-binding' on a line other
                    ;; than the first.
                    (and (assq 'lexical-binding file-local-variables-alist)
                         (null (cdr (assq 'lexical-binding (flypkg/get-header-line-file-local-variables))))))
            (flypkg/error
             context 1 1 'error
             "`lexical-binding' must be set in the first line.")))))))

(flypkg/define-pass flypkg/do-not-depend-on-cl-lib-1.0 (context)
  "Check that any dependency on \"cl-lib\" is on a remotely-installable version."
  (pcase-let ((`(,line-no . ,valid-deps)
               (flypkg/call-pass context #'flypkg/get-well-formed-dependencies)))
    (let ((cl-lib-dep (assq 'cl-lib valid-deps)))
      (when cl-lib-dep
        (let ((cl-lib-version (nth 1 cl-lib-dep)))
          (when (version-list-<= '(1) cl-lib-version)
            (flypkg/error
             context line-no (nth 2 cl-lib-dep) 'error
             (format "Depend on the latest 0.x version of cl-lib rather than on version \"%S\".
Alternatively, depend on Emacs 24.3, which introduced cl-lib 1.0."
                     cl-lib-version))))))))

(flypkg/define-pass flypkg/valid-package-version-present (context)
  "Check that a valid \"Version\" header is present."
  (flypkg/call-pass context #'flypkg/get-dependency-list)
  (let ((version (lm-header (rx (? "Package-") "Version"))))
    (if version
        (unless (ignore-errors (version-to-list version))
          (flypkg/error
           context
           (line-number-at-pos)
           (1+ (- (match-beginning 1) (line-beginning-position)))
           'warning
           (format "\"%s\" is not a valid version. MELPA will handle this, but other archives will not." version)))
      (flypkg/error
       context 1 1 'warning
       "\"Version:\" or \"Package-Version:\" header is missing. MELPA will handle this, but other archives will not."))))

(flypkg/define-pass flypkg/package-el-can-parse-buffer (context)
  "Check that `package-buffer-info' can read metadata from this file."
  (flypkg/call-pass context #'flypkg/valid-package-version-present)
  (condition-case err
      (let ((orig-buffer (current-buffer)))
        ;; We've reported version header issues separately, so rule them out here
        (with-temp-buffer
          (insert-buffer-substring-no-properties orig-buffer)
          (flypkg/update-or-insert-version "0")
          (package-buffer-info)))
    (error
     (flypkg/error
      context
      1 1
      'error
      (format "package.el cannot parse this buffer: %s" (error-message-string err))))))


;;; Helpers and checker definition

(defun flypkg/lowest-installable-version-of (package)
  "Return the lowest version of PACKAGE available for installation."
  (let ((descriptors (cdr (assq package package-archive-contents))))
    (if (fboundp 'package-desc-version)
        (car (sort (mapcar 'package-desc-version descriptors)
                   #'version-list-<))
      (aref descriptors 0))))

(defun flypkg/goto-header (header-name)
  "Move to the first occurrence of HEADER-NAME in the file.
If the return value is non-nil, then point will be at the end of
the file, and the second and third match groups will contain the name and
value of the header with any leading or trailing whitespace removed."
  (let ((initial-point (point)))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward (concat (lm-get-header-re header-name) "\\(.*?\\) *$") nil t)
          (point)
        (goto-char initial-point)
        nil))))

(defun flypkg/update-or-insert-version (version)
  "Ensure current buffer has a \"Version: VERSION\" header."
  (if (flypkg/goto-header "Version")
      (move-beginning-of-line nil)
    (forward-line))
  (insert (format ";; Version: %s" version))
  (newline))

(defun flypkg/get-header-line-file-local-variables ()
  "Return local variables specified in the -*- line.
Returns an alist of elements (VAR . VAL), where VAR is a variable
and VAL is the specified value.

For details, see `hack-local-variables-prop-line'."
  (save-excursion
    (goto-char (point-min))
    (cl-letf (((symbol-function #'message) #'ignore))
      (hack-local-variables-prop-line))))

(flycheck-define-generic-checker 'emacs-lisp-package
  "A checker for \"Package-Requires\" headers."
  :start #'flypkg/start
  :modes '(emacs-lisp-mode))

;;;###autoload
(defun flycheck-package-setup ()
  "Setup flycheck-package.
Add `flycheck-emacs-lisp-package' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'emacs-lisp-package t)
  (flycheck-add-next-checker 'emacs-lisp 'emacs-lisp-package t)
  (flycheck-add-next-checker 'emacs-lisp-checkdoc 'emacs-lisp-package t))

(provide 'flycheck-package)
;;; flycheck-package.el ends here
