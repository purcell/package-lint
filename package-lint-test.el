;;; package-lint-test.el --- package-lint test suite

;; Copyright (C) 2016-2017  Steve Purcell, Fanael Linithien

;; Author: Steve Purcell <steve@sanityinc.com>
;;         Fanael Linithien <fanael4@gmail.com>
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

;;

;;; Code:
(require 'package-lint)
(require 'ert)

(defun package-lint-test--run (contents &optional header version footer)
  "Run `package-lint-buffer' on a temporary buffer with given CONTENTS.

HEADER, VERSION and FOOTER can be either strings or nil; when one is a string,
the corresponding package boilerplate part is replaced with the passed string,
when it's nil, the default is used."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert (or header ";;; test.el --- A test\n"))
    (insert (or version ";; Package-Version: 0\n"))
    (insert contents)
    (insert (or footer "\n\n;;; test.el ends here\n"))
    (let ((buffer-file-name "test.el"))
      (package-lint-buffer))))

(ert-deftest package-lint-test-accept-standard-keywords ()
  ;; Test comma- and space-separated keywords, as both are commonly used.
  (should (equal '() (package-lint-test--run ";; Keywords: lisp convenience")))
  (should (equal '() (package-lint-test--run ";; Keywords: lisp, convenience"))))

(ert-deftest package-lint-test-warn-no-standard-keyword ()
  (should
   (equal
    '((3 1 warning "You should include standard keywords: see the variable `finder-known-keywords'."))
    (package-lint-test--run ";; Keywords: foo"))))

(ert-deftest package-lint-test-no-warning-if-at-least-one-standard-keyword ()
  (should
   (equal nil (package-lint-test--run ";; Keywords: lisp foo"))))

(ert-deftest package-lint-test-warn-invalid-version ()
  (should
   (member
    '(2 21 warning "\"invalid\" is not a valid version. MELPA will handle this, but other archives will not.")
    (package-lint-test--run "" nil ";; Package-Version: invalid"))))

(ert-deftest package-lint-test-warn-no-version ()
  (should
   (member
    '(1 1 warning "\"Version:\" or \"Package-Version:\" header is missing. MELPA will handle this, but other archives will not.")
    (package-lint-test--run ";; Package-Requires: ((example \"0\"))" nil ""))))

(ert-deftest package-lint-test-accept-valid-version ()
  (should (equal '() (package-lint-test--run "" nil ";; Package-Version: 1.2.3-cvs"))))

(ert-deftest package-lint-test-error-lexical-binding-not-at-end ()
  (should
   (equal
    '((1 1 error "`lexical-binding' must be set in the first line."))
    (package-lint-test--run
     ";; Local Variables:
;; lexical-binding: t
;; End:"))))

(ert-deftest package-lint-test-warn-lexical-binding-without-emacs-24-dep ()
  (should
   (equal
    '((1 28 warning "You should depend on (emacs \"24\") if you need lexical-binding."))
    (package-lint-test--run
     ""
     ";;; test.el --- A test -*- lexical-binding: t -*-\n"))))

(ert-deftest package-lint-test-accept-lexical-binding-with-emacs-24-dep ()
  (should
   (equal
    '()
    (package-lint-test--run
     ";; Package-Requires: ((emacs \"24\"))"
     ";;; test.el --- A test -*- lexical-binding: t -*-\n"))))

(ert-deftest package-lint-test-warn-empty-summary ()
  (should
   (equal
    '((1 1 warning "Package should have a non-empty summary."))
    (package-lint-test--run "" ";;; test.el ---\n"))))

(ert-deftest package-lint-test-warn-too-long-summary ()
  (should
   (equal
    '((1 1 warning "The package summary is too long. It should be at most 50 characters."))
    (package-lint-test--run
     ""
     ";;; test.el --- xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n"))))

(ert-deftest package-lint-test-warn-emacs-in-summary ()
  (should
   (equal
    '((1 1 warning "Including \"Emacs\" in the package description is usually redundant."))
    (package-lint-test--run "" ";;; test.el --- A package for Emacs\n"))))

(ert-deftest package-lint-test-accept-emacs-lisp-in-summary ()
  (should (equal '() (package-lint-test--run "" ";;; test.el --- Emacs Lisp test framework\n"))))

(ert-deftest package-lint-test-error-invalid-dependency ()
  (should
   (member
    '(3 1 error "Expected (package-name \"version-num\"), but found invalid.")
    (package-lint-test--run ";; Package-Requires: (invalid)")))
  (should
   (member
    '(3 24 error "\"invalid\" is not a valid version string: see `version-to-list'.")
    (package-lint-test--run ";; Package-Requires: ((package-lint \"invalid\"))"))))

(ert-deftest package-lint-test-error-emacs-23-dep ()
  (should
   (equal
    '((3 24 error "You can only depend on Emacs version 24 or greater: package.el for Emacs 23 does not support the \"emacs\" pseudopackage."))
    (package-lint-test--run ";; Package-Requires: ((emacs \"23\"))"))))

(ert-deftest package-lint-test-accept-emacs-24+-dep ()
  (should (equal '() (package-lint-test--run ";; Package-Requires: ((emacs \"24\"))")))
  (should (equal '() (package-lint-test--run ";; Package-Requires: ((emacs \"26.7\"))"))))

(ert-deftest package-lint-test-error-uninstallable-dep ()
  (should
   (equal
    '((3 24 error "Package example-nonexistent-package is not installable."))
    (package-lint-test--run ";; Package-Requires: ((example-nonexistent-package \"1\"))"))))

(ert-deftest package-lint-test-warn-snapshot-dep ()
  (should
   (equal
    '((3 24 warning "Use a non-snapshot version number for dependency on \"package-lint\" if possible."))
    (package-lint-test--run ";; Package-Requires: ((package-lint \"20160101.1234\"))"))))

(ert-deftest package-lint-test-warn-unversioned-dep ()
  (should
   (equal
    '((3 24 warning "Use a properly versioned dependency on \"package-lint\" if possible."))
    (package-lint-test--run ";; Package-Requires: ((package-lint \"0\"))"))))

(ert-deftest package-lint-test-error-cl-lib-1.0-dep ()
  (should
   (member
    '(3 24 error "Depend on the latest 0.x version of cl-lib rather than on version \"(1)\".
Alternatively, depend on (emacs \"24.3\") or greater, in which cl-lib is bundled.")
    (package-lint-test--run ";; Package-Requires: ((cl-lib \"1\"))"))))

(ert-deftest package-lint-test-accept-normal-deps ()
  (should (equal '() (package-lint-test--run
                      ";; Package-Requires: ((package-lint \"0.2\") (cl-lib \"0.5\"))"))))

(ert-deftest package-lint-test-error-new-functions ()
  (should
   (equal
    '((3 1 error "You should depend on (emacs \"25\") if you need `when-let'."))
    (package-lint-test--run
     "(when-let ((foo (bar))) (message \"ok\"))"))))

(ert-deftest package-lint-test-accept-new-functions-with-dep ()
  (should
   (equal
    '()
    (package-lint-test--run
     ";; Package-Requires: ((emacs \"25\"))
\(when-let ((foo (bar))) (message \"ok\"))"))))

(ert-deftest package-lint-test-error-nonstandard-symbol-separator ()
  (should
   (equal
    '((4 1 error "`foo:bar' contains a non-standard separator `:', use hyphens instead.")
      (3 1 error "`foo/bar' contains a non-standard separator `/', use hyphens instead."))
    (package-lint-test--run
     "(defun foo/bar () t)\n(defun foo:bar () nil)")))
  ;; But accept /= when at the end.
  (should (equal '() (package-lint-test--run "(defun foo-/= (a b) t)"))))

(ert-deftest package-lint-test-error-unprefixed-definitions ()
  (should
   (equal
    '((3 1 error "\"foo\" doesn't start with package's prefix \"test\"."))
    (package-lint-test--run "(defun foo ())\n(provide 'test)"))))

(ert-deftest package-lint-test-accept-prefixed-definitions ()
  (should (equal '() (package-lint-test--run
                      "(defun test-foo ())\n(defun test ())\n(provide 'test)"))))

(ert-deftest package-lint-test-accept-sane-prefixed-definitions ()
  (should (equal '() (package-lint-test--run
                      "(defun org-dblock-write:test ())\n(provide 'test)"))))

(ert-deftest package-lint-test-error-new-libraries ()
  (should
   (equal
    '((3 10 error "You should depend on (emacs \"24.4\") if you need `nadvice'."))
    (package-lint-test--run "(require 'nadvice)"))))

(ert-deftest package-lint-test-accept-new-libraries-with-dep ()
  (should (equal '() (package-lint-test--run
                      ";; Package-Requires: ((emacs \"24.4\"))\n(require 'nadvice)"))))

(ert-deftest package-lint-test-accept-new-libraries-with-optional-require ()
  (should (equal '() (package-lint-test--run "(require 'nadvice nil t)"))))

(ert-deftest package-lint-test-looks-like-a-package-p-works ()
  (should
   (with-temp-buffer
     (insert ";; Package-Version: 0\n")
     (package-lint-looks-like-a-package-p)))
  (should
   (with-temp-buffer
     (insert ";; Package-Requires: ((foo \"1\"))\n")
     (package-lint-looks-like-a-package-p)))
  (should
   (with-temp-buffer
     (insert ";; Version: 0\n")
     (package-lint-looks-like-a-package-p)))
  (should-not (with-temp-buffer (package-lint-looks-like-a-package-p)))
  (should-not
   (with-temp-buffer
     (insert ";; Dummy-Header: dummy-value\n")
     (package-lint-looks-like-a-package-p))))

(provide 'package-lint-test)
;;; package-lint-test.el ends here
