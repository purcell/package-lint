;;; package-lint-test.el --- Test suite for package-lint

;; Copyright (C) 2016-2019  Steve Purcell, Fanael Linithien

;; Author: Steve Purcell <steve@sanityinc.com>
;;         Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/purcell/package-lint
;; Version: 0
;; Package-Requires: ((emacs "24.1") (cl-lib "0.6"))

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

;; This file contains the official package-lint testsuite.

;;; Code:
(require 'package-lint)
(require 'ert)
(require 'cl-lib)

(defun package-lint-test-add-package-lint-foobar-to-archive (version &optional archive)
  "Add a package-lint-foobar package to ARCHIVE.
ARCHIVE defaults to \"melpa-stable\".
VERSION is a list of numbers, e.g., (0 5 0) to represent version
0.5.0."
  (package--add-to-archive-contents
   `(package-lint-foobar . [,version ((emacs (25 1))) "Some documentation" single
                                     ((:commit . "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                                      (:url . "https://gitlab.com/somewhere"))])
   (or archive "melpa-stable")))

(cl-defun package-lint-test--run (contents &key header version footer provide commentary url featurename)
  "Run `package-lint-buffer' on a temporary buffer with given CONTENTS.

HEADER, VERSION, FOOTER, PROVIDE, COMMENTARY, and URL can be
either strings or nil; when one is a string, the corresponding
package boilerplate part is replaced with the passed string, when
it's nil, the default is used.

FEATURENAME defaults to \"test\", and is used in the file name,
headers and provide form."
  (unless featurename
    (setq featurename "test"))
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert (or header (format ";;; %s.el --- A test\n" featurename)))
    (insert (or version ";; Package-Version: 0\n"))
    (insert (or url ";; URL: https://package-lint.test/p?q#f\n"))
    (insert (or commentary ";;; Commentary:\n;; A test package, for testing.\n"))
    (insert contents)
    (insert "\n" (or provide (format "(provide '%s)\n" featurename)))
    (insert (or footer (format "\n\n;;; %s.el ends here\n" featurename)))
    (let ((buffer-file-name (format "%s.el" featurename)))
      (package-lint-buffer))))

(ert-deftest package-lint-test-reserved-keybindings ()
  (let ((reserved-message "This key sequence is reserved (see Key Binding Conventions in the Emacs Lisp manual)"))
    ;; C-c and a letter (either upper or lower case)
    (should (equal (package-lint-test--run "(kbd \"C-c n\")")
                   `((6 13 error ,reserved-message))))
    (should (equal (package-lint-test--run "(local-set-key \"\\C-cF\" 'something)")
                   `((6 34 error ,reserved-message))))
    (should-not (package-lint-test--run "(kbd \"C-d n\")"))
    (should (equal (package-lint-test--run "(kbd \"C-c x n\")")
                   `((6 15 error ,reserved-message))))

    ;; [C-keyname] bindings should work fine
    (should-not (package-lint-test--run "(define-key map [C-return] 'something)"))

    ;; C-c followed by a control character or a digit
    (should-not (package-lint-test--run "(defcustom test-something (kbd \"C-c 1\"))"))
    (should-not (package-lint-test--run "(global-set-key \"\\C-c1\" 'something)"))

    ;; C-c followed by {, }, <, >, : or ;
    (should-not (package-lint-test--run "(defcustom test-something (kbd \"C-c <\"))"))
    (should-not (package-lint-test--run "(define-key map \"\\C-c<\" 'something)"))

    ;; Function keys <F5> through <F9> without modifier keys
    (should (equal (package-lint-test--run "(define-key map (kbd \"<f5>\") 'something)")
                   `((6 40 error ,reserved-message))))
    (should (equal (package-lint-test--run "(global-set-key [f5] 'something)")
                   `((6 32 error ,reserved-message))))
    (should-not (package-lint-test--run "(global-set-key [f4] 'something)"))
    (should-not (package-lint-test--run "(global-set-key (kbd \"C-c <tab>\") 'something)"))

    ;; C-c followed by any other ASCII punctuation or symbol character
    (should-not (package-lint-test--run "(defcustom test-something (kbd \"C-c .\"))"))
    (should-not (package-lint-test--run "(global-set-key \"\\C-c.\" 'something)"))

    ;; But C-c followed by another modifier sequence is allowed
    (should-not (package-lint-test--run "(global-set-key (kbd \"C-c C-x d\") 'something)"))

    ;; Don't bind C-h following any prefix character
    (should (equal (package-lint-test--run "(defcustom test-something (kbd \"C-x C-h\"))")
                   `((6 41 error ,reserved-message))))
    (should-not (package-lint-test--run "(defcustom test-something (kbd \"C-h C-x\"))"))

    ;; Don't bind a key sequence ending in <C-g>
    (should (equal (package-lint-test--run "(defcustom test-something (kbd \"C-x C-g\"))")
                   `((6 41 error ,reserved-message))))
    (should (equal (package-lint-test--run "(global-set-key \"\\C-c\\C-g\" 'something)")
                   `((6 38 error ,reserved-message))))
    (should-not (package-lint-test--run "(global-set-key \"C-x g\" 'something)"))
    ;; But we allow C-g alone, which is acceptable for some special cases
    (should-not (package-lint-test--run "(defcustom test-something (kbd \"C-g\"))"))

    ;; Don't bind a key sequence ending in <ESC> except following another <ESC>
    (should (equal (package-lint-test--run "(defcustom test-something (kbd \"C-x <ESC>\")")
                   `((6 43 error ,reserved-message))))
    (should-not (package-lint-test--run "(defcustom test-something (kbd \"C-x <ESC> <ESC>\"))"))))

(ert-deftest package-lint-test-error-autoloads-on-private-functions ()
  (should (equal '() (package-lint-test--run "(defun test--private-function ())")))
  (should
   (equal
    '((6 0 warning "Private functions generally should not be autoloaded."))
    (package-lint-test--run ";;;###autoload\n(defun test--private-function ())")))
  (should
   (equal
    '((6 0 warning "Private functions generally should not be autoloaded."))
    (package-lint-test--run ";;;###autoload\n(defmacro test--private-macro ())"))))

(ert-deftest package-lint-test-warn-literal-emacs-path ()
  (should
   (equal
    '((6 9 warning "Use variable `user-emacs-directory' or function `locate-user-emacs-file' instead of a literal path to the Emacs user directory or files."))
    (package-lint-test--run "\".emacs\.d\"")))
  (should
   (equal
    '((6 11 warning "Use variable `user-emacs-directory' or function `locate-user-emacs-file' instead of a literal path to the Emacs user directory or files."))
    (package-lint-test--run "\"~/.emacs\.d/foo\"")))
  (should (equal '() (package-lint-test--run "\"/foo/foo.emacs.dat\"")))
  (should (equal '() (package-lint-test--run ";; ~/\.emacs\.d/elpa")))
  (should (equal '() (package-lint-test--run "\"emacs dot dee\""))))

(ert-deftest package-lint-test-accept-standard-keywords ()
  ;; Test comma- and space-separated keywords, as both are commonly used.
  (should (equal '() (package-lint-test--run ";; Keywords: lisp convenience")))
  (should (equal '() (package-lint-test--run ";; Keywords: lisp, convenience"))))

(ert-deftest package-lint-test-warn-no-standard-keyword ()
  (should
   (equal
    '((6 3 warning "You should include standard keywords: see the variable `finder-known-keywords'."))
    (package-lint-test--run ";; Keywords: foo"))))

(ert-deftest package-lint-test-no-warning-if-at-least-one-standard-keyword ()
  (should
   (equal nil (package-lint-test--run ";; Keywords: lisp foo"))))

(ert-deftest package-lint-test-error-if-no-url ()
  (should
   (equal
    '((1 0 error "Package should have a Homepage or URL header."))
    (package-lint-test--run "" :url "")))
  (should
   (equal
    '((3 8 error "Package URLs should be a single HTTPS or HTTP URL."))
    (package-lint-test--run "" :url ";; URL: not a URL\n")))
  (should
   (equal
    '((3 8 error "Package URLs should be a single HTTPS or HTTP URL."))
    (package-lint-test--run "" :url ";; URL: git://test/test.git\n"))))

(ert-deftest package-lint-test-accept-homepage ()
  (should (equal '() (package-lint-test--run "" :url ";; Homepage: https://package-lint.test/foo\n"))))

(ert-deftest package-lint-test-warn-invalid-version ()
  (should
   (member
    '(2 20 warning "\"invalid\" is not a valid version. MELPA will handle this, but other archives will not.")
    (package-lint-test--run "" :version ";; Package-Version: invalid\n"))))

(ert-deftest package-lint-test-warn-no-version ()
  (should
   (member
    '(1 0 warning "\"Version:\" or \"Package-Version:\" header is missing. MELPA will handle this, but other archives will not.")
    (package-lint-test--run ";; Package-Requires: ((example \"0\"))" :version ""))))

(ert-deftest package-lint-test-accept-valid-version ()
  (should (equal '() (package-lint-test--run "" :version ";; Package-Version: 1.2.3-cvs\n"))))

(ert-deftest package-lint-test-error-lexical-binding-not-at-end ()
  (should
   (equal
    '((1 0 error "`lexical-binding' must be set in the first line."))
    (package-lint-test--run
     ";; Local Variables:
;; lexical-binding: t
;; End:"))))

(ert-deftest package-lint-test-warn-lexical-binding-without-emacs-24-dep ()
  (should
   (equal
    '((1 27 warning "You should depend on (emacs \"24.1\") if you need lexical-binding."))
    (package-lint-test--run
     ""
     :header ";;; test.el --- A test -*- lexical-binding: t -*-\n"))))

(ert-deftest package-lint-test-accept-lexical-binding-with-emacs-24-dep ()
  (should
   (equal
    '()
    (package-lint-test--run
     ";; Package-Requires: ((emacs \"24\"))"
     :header ";;; test.el --- A test -*- lexical-binding: t -*-\n"))))

(ert-deftest package-lint-test-warn-empty-summary ()
  (should
   (equal
    '((1 0 error "Package should have a non-empty summary."))
    (package-lint-test--run "" :header ";;; test.el ---\n"))))

(ert-deftest package-lint-test-warn-too-long-summary ()
  (should
   (equal
    '((1 0 warning "The package summary is too long. It should be at most 60 characters.")
      (1 0 warning "The package summary should start with an uppercase letter or a digit."))
    (package-lint-test--run
     ""
     :header ";;; test.el --- xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n"))))

(ert-deftest package-lint-test-warn-summary-end-with-period ()
  (should
   (equal
    '((1 0 warning "The package summary should not end with a period."))
    (package-lint-test--run
     ""
     :header ";;; test.el --- A test.\n"))))

(ert-deftest package-lint-test-warn-emacs-in-summary ()
  (should
   (equal
    '((1 0 warning "Including \"Emacs\" in the package summary is usually redundant."))
    (package-lint-test--run "" :header ";;; test.el --- A package for Emacs\n"))))

(ert-deftest package-lint-test-accept-emacs-lisp-in-summary ()
  (should (equal '() (package-lint-test--run "" :header ";;; test.el --- Emacs Lisp test framework\n"))))

(ert-deftest package-lint-test-accept-.emacs-in-summary ()
  (should (equal '() (package-lint-test--run "" :header ";;; test.el --- Something for .emacs\n"))))

(ert-deftest package-lint-test-error-invalid-dependency ()
  (should
   (member
    '(6 23 error "\"invalid\" is not a valid version string: see `version-to-list'.")
    (package-lint-test--run ";; Package-Requires: ((package-lint \"invalid\"))"))))

(ert-deftest package-lint-test-error-emacs-23-dep ()
  (should
   (equal
    '((6 23 error "You can only depend on Emacs version 24 or greater: package.el for Emacs 23 does not support the \"emacs\" pseudopackage."))
    (package-lint-test--run ";; Package-Requires: ((emacs \"23\"))"))))

(ert-deftest package-lint-test-warning-emacs-head-dep ()
  (should
   (equal
    '((6 23 warning "This makes the package uninstallable in all released Emacs versions."))
    (package-lint-test--run ";; Package-Requires: ((emacs \"29\"))"))))

(ert-deftest package-lint-test-accept-emacs-24+-dep ()
  (should (equal '() (package-lint-test--run ";; Package-Requires: ((emacs \"24\"))")))
  (should (equal '() (package-lint-test--run ";; Package-Requires: ((emacs \"26.3\"))"))))

(ert-deftest package-lint-test-error-uninstallable-dep ()
  (should
   (equal
    '((6 23 error "Package example-nonexistent-package is not installable."))
    (package-lint-test--run ";; Package-Requires: ((example-nonexistent-package \"1\"))"))))

(ert-deftest package-lint-test-warn-snapshot-dep ()
  (should
   (equal
    '((6 23 warning "Use a non-snapshot version number for dependency on \"package-lint\" if possible."))
    (package-lint-test--run ";; Package-Requires: ((package-lint \"20160101.1234\"))"))))

(ert-deftest package-lint-test-warn-unversioned-dep ()
  (should
   (member
    '(6 22 warning "Use a properly versioned dependency on \"package-lint\" if possible.")
    (package-lint-test--run ";; Package-Requires: (package-lint)")))
  (should
   (member
    '(6 23 warning "Use a properly versioned dependency on \"package-lint\" if possible.")
    (package-lint-test--run ";; Package-Requires: ((package-lint))")))
  (should
   (equal
    '((6 23 warning "Use a properly versioned dependency on \"package-lint\" if possible."))
    (package-lint-test--run ";; Package-Requires: ((package-lint \"0\"))"))))

(ert-deftest package-lint-test-warn-dependency-too-high ()
  (let ((package-archive-contents nil))
    (package-lint-test-add-package-lint-foobar-to-archive '(0 5 0))
    (should
     (equal
      '((6 23 warning "Version dependency for package-lint-foobar appears too high: try 0.5.0"))
      (package-lint-test--run ";; Package-Requires: ((package-lint-foobar \"0.6.0\"))")))))

(ert-deftest package-lint-test-dont-warn-dependency-too-high-1 ()
  (let ((package-archive-contents nil))
    (package-lint-test-add-package-lint-foobar-to-archive '(0 5 0))
    (should
     (equal '() (package-lint-test--run ";; Package-Requires: ((package-lint-foobar \"0.5.0\"))")))))

(ert-deftest package-lint-test-dont-warn-dependency-too-high-2 ()
  (let ((package-archive-contents nil))
    (package-lint-test-add-package-lint-foobar-to-archive '(0 6 0))
    (should
     (equal '() (package-lint-test--run ";; Package-Requires: ((package-lint-foobar \"0.5.0\"))")))))

(ert-deftest package-lint-test-dont-warn-dependency-too-high-3 ()
  (let ((package-archive-contents nil))
    ;; Sometimes a package appears with different versions from
    ;; different archives:
    (package-lint-test-add-package-lint-foobar-to-archive '(0 5 0) "gnu")
    (package-lint-test-add-package-lint-foobar-to-archive '(0 1 0) "melpa-stable")
    (should
     (equal '() (package-lint-test--run ";; Package-Requires: ((package-lint-foobar \"0.3.0\"))")))))

(ert-deftest package-lint-test-error-cl-lib-1.0-dep ()
  (should
   (member
    '(6 23 error "Depend on the latest 0.x version of cl-lib rather than on version \"(1)\".
Alternatively, depend on (emacs \"24.3\") or greater, in which cl-lib is bundled.")
    (package-lint-test--run ";; Package-Requires: ((cl-lib \"1\"))"))))

(ert-deftest package-lint-test-warning-cl-lib-not-needed ()
  (should
   (member
    '(6 52 warning "An explicit dependency on cl-lib <= 1.0 is not needed on Emacs >= 24.3.")
    (package-lint-test--run ";; Package-Requires: ((emacs \"25.1\") (cl-lib \"1.0\"))")))
  (should
   (member
    '(6 52 warning "An explicit dependency on cl-lib <= 1.0 is not needed on Emacs >= 24.3.")
    (package-lint-test--run ";; Package-Requires: ((emacs \"24.3\") (cl-lib \"0.5\"))"))))

(ert-deftest package-lint-test-warning-cl ()
  (should
   (member
    '(6 10 warning "Replace deprecated `cl' with `cl-lib'.  The `cl-libify' package can help with this.")
    (package-lint-test--run "(require 'cl)"))))

(ert-deftest package-lint-test-warning-cl-macs-etc ()
  (should
   (member
    '(6 10 warning "This file is not in the `cl-lib' ELPA compatibility package: require `cl-lib' instead.")
    (package-lint-test--run "(require 'cl-macs)")))
  (should
   (member
    '(6 10 warning "This file is not in the `cl-lib' ELPA compatibility package: require `cl-lib' instead.")
    (package-lint-test--run "(require 'cl-seq)"))))

(ert-deftest package-lint-test-accept-normal-deps ()
  (should (equal '() (package-lint-test--run
                      ";; Package-Requires: ((package-lint \"0.2\") (cl-lib \"0.5\"))"))))

(ert-deftest package-lint-test-error-new-functions ()
  (should
   (equal
    '((6 1 error "You should depend on (emacs \"25.1\") if you need `when-let'."))
    (package-lint-test--run
     "(when-let ((foo (bar))) (message \"ok\"))"))))

(ert-deftest package-lint-test-error-new-functions-as-quote ()
  (should
   (equal
    '((6 20 error "You should depend on (emacs \"24.1\") if you need `window-resize'."))
    (package-lint-test--run
     "(defconst test-fn #'window-resize)"))))

(ert-deftest package-lint-test-error-new-functions-as-arg ()
  (should
   (equal
    '((6 10 error "You should depend on (emacs \"24.1\") if you need `window-resize'."))
    (package-lint-test--run
     "(funcall 'window-resize foo)")))
  (should
   (equal
    '((6 11 error "You should depend on (emacs \"24.1\") if you need `window-resize'."))
    (package-lint-test--run
     "(funcall #'window-resize foo)")))
  (should
   (equal
    '((6 8 error "You should depend on (emacs \"24.1\") if you need `window-resize'."))
    (package-lint-test--run
     "(apply 'window-resize foo)")))
  (should
   (equal
    '((6 9 error "You should depend on (emacs \"24.1\") if you need `window-resize'."))
    (package-lint-test--run
     "(apply #'window-resize foo)")))
  (should
   (equal
    '((6 13 error "You should depend on (emacs \"24.1\") if you need `window-resize'."))
    (package-lint-test--run
     "(advice-add 'window-resize foo)"))))

(ert-deftest package-lint-test-accept-new-functions-with-dep ()
  (should
   (equal
    '()
    (package-lint-test--run
     ";; Package-Requires: ((emacs \"25.1\"))
\(when-let ((foo (bar))) (message \"ok\"))"))))

(ert-deftest package-lint-test-accept-new-functions-with-fboundp ()
  (should
   (equal
    '()
    (package-lint-test--run
     "(if (fboundp 'when-let)
    (when-let blah)
  (bloop))"))))

(ert-deftest package-lint-test-error-new-backported-functions ()
  (should
   (equal
    '((6 1 error "You should depend on (emacs \"25.1\") or the seq package if you need `seq-length'."))
    (package-lint-test--run
     "(seq-length '(foo))"))))

(ert-deftest package-lint-test-accepts-new-backported-functions-with-emacs-dep ()
  (should
   (equal
    '()
    (package-lint-test--run
     ";; Package-Requires: ((emacs \"25.1\"))
\(seq-length '(foo))"))))

(ert-deftest package-lint-test-accepts-new-backported-functions-with-backport-dep ()
  (should
   (equal
    '()
    (package-lint-test--run
     ";; Package-Requires: ((seq \"1\"))
\(seq-length '(foo))"))))

(ert-deftest package-lint-test-error-nonstandard-symbol-separator ()
  (should
   (equal
    '((6 0 error "`test-thing/bar' contains a non-standard separator `/', use hyphens instead (see Elisp Coding Conventions).")
      (7 0 error "`test-thing:bar' contains a non-standard separator `:', use hyphens instead (see Elisp Coding Conventions).")
      (8 0 error "`test-thing:face' contains a non-standard separator `:', use hyphens instead (see Elisp Coding Conventions).")
      (9 0 error "`test-thing/face' contains a non-standard separator `/', use hyphens instead (see Elisp Coding Conventions)."))
    (package-lint-test--run
     "\
(defun test-thing/bar () t)
(defun test-thing:bar () nil)
(defface test-thing:face '((default)))
(defface test-thing/face '((default)))")))
  ;; But accept /= or / when at the end.
  (should (equal '() (package-lint-test--run "(defun test-/= (a b) t)")))
  (should (equal '() (package-lint-test--run "(defun test-/ (a b) t)"))))

(ert-deftest package-lint-test-error-unprefixed-definitions ()
  (should
   (equal
    '((6 0 error "\"foo\" doesn't start with package's prefix \"test\"."))
    (package-lint-test--run "(defvar foo 'hello)")))
  (should
   (equal
    '((6 0 error "\"foo\" doesn't start with package's prefix \"test\"."))
    (package-lint-test--run "(defun foo ())")))
  (should
   (equal
    '((6 0 error "\"global-testfoo-mode\" doesn't start with package's prefix \"test\"."))
    (package-lint-test--run "(define-globalized-minor-mode global-testfoo-mode ignore ignore :require 'test)"))))

;; (ert-deftest package-lint-test-accept-forward-var-declaration ()
;;   (should
;;    (equal
;;     nil ;; Or, probably, an "info" message
;;     (package-lint-test--run "(defvar foo)"))))

(ert-deftest package-lint-test-accept-prefixed-definitions ()
  (should (equal '() (package-lint-test--run
                      "(defun test-foo ())\n(defun test ())")))
  (should (equal '() (package-lint-test--run
                      "(define-globalized-minor-mode global-test-mode ignore ignore :require 'test)")))
  (should (equal '() (package-lint-test--run
                      "(define-globalized-minor-mode global-test-foo-mode ignore ignore :require 'test)"))))

(ert-deftest package-lint-test-accept-sane-prefixed-definitions ()
  (should (equal '() (package-lint-test--run
                      "(defun org-dblock-write:test ())"))))

(ert-deftest package-lint-test-error-redefining-builtins ()
  (should
   (equal
    '((8 0 error "Define compatibility functions with a prefix, e.g. \"test--setq-local\", and use `defalias' where they exist."))
    (package-lint-test--run "(eval-when-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      (list 'set (list 'make-local-variable (list 'quote var)) val))))"))))

(ert-deftest package-lint-test-error-new-libraries ()
  (should
   (equal
    '((6 10 error "You should depend on (emacs \"24.4\") if you need `subr-x'."))
    (package-lint-test--run "(require 'subr-x)"))))

(ert-deftest package-lint-test-error-removed-libraries ()
  (should
   (equal
    '((6 10 error "The `awk-mode' library was removed in Emacs version 26.1."))
    (package-lint-test--run "(require 'awk-mode)"))))

(ert-deftest package-lint-test-error-removed-functions ()
  (should
   (equal
    '((6 1 error "`spell-buffer' was removed in Emacs version 26.1."))
    (package-lint-test--run "(spell-buffer)"))))

(ert-deftest package-lint-test-allow-removed-and-readded-functions ()
  ;; History of mm-url-encode-multipart-form-data:
  ;; * function-added in Emacs (25 1)
  ;; * function-removed in Emacs (24 3)
  ;; * function-added in Emacs (24 1)
  (should
   (equal
    '((6 1 error "You should depend on (emacs \"25.1\") if you need `mm-url-encode-multipart-form-data'.")
      (6 1 error "`mm-url-encode-multipart-form-data' was removed in Emacs version 24.3."))
    (package-lint-test--run "(mm-url-encode-multipart-form-data)")))
  (equal
   ;; This is clunky, but won't currently fix
   '((6 1 error "You should depend on (emacs \"25.1\") if you need `mm-url-encode-multipart-form-data'.")
     (6 1 error "`mm-url-encode-multipart-form-data' was removed in Emacs version 24.3."))
   (package-lint-test--run ";; Package-Requires: ((emacs \"24.1\"))\n(mm-url-encode-multipart-form-data)"))  (should
   (equal
    '()
    (package-lint-test--run ";; Package-Requires: ((emacs \"25.1\"))\n(mm-url-encode-multipart-form-data)"))))

(ert-deftest package-lint-test-accept-new-libraries-with-dep ()
  (should (equal '() (package-lint-test--run
                      ";; Package-Requires: ((emacs \"24.4\"))\n(require 'subr-x)"))))

(ert-deftest package-lint-test-error-new-backported-libraries ()
  (should
   (equal
    '((6 10 error "You should depend on (emacs \"25.1\") or the seq package if you need `seq'."))
    (package-lint-test--run "(require 'seq)"))))

(ert-deftest package-lint-test-error-new-backported-sub-libraries ()
  (should
   (equal
    '((6 10 error "You should depend on (emacs \"27.1\") or the (org \"9.3\") package if you need `ol'."))
    (package-lint-test--run "(require 'ol)")))
  (should
   (equal
    '((7 10 error "You should depend on (emacs \"27.1\") or the (org \"9.3\") package if you need `ol'."))
    (package-lint-test--run ";; Package-Requires: ((org \"9.2\"))\n(require 'ol)"))))

(ert-deftest package-lint-test-accept-new-backported-libraries-with-emacs-dep ()
  (should (equal '() (package-lint-test--run
                      ";; Package-Requires: ((emacs \"25.1\"))\n(require 'seq)"))))

(ert-deftest package-lint-test-accept-new-backported-libraries-with-backport-dep ()
  (should (equal '() (package-lint-test--run
                      ";; Package-Requires: ((seq \"1\"))\n(require 'seq)"))))

(ert-deftest package-lint-test-accept-new-backported-sub-libraries-with-emacs-dep ()
  (should (equal '() (package-lint-test--run
                      ";; Package-Requires: ((emacs \"27.1\"))\n(require 'ol)"))))

(ert-deftest package-lint-test-accept-new-backported-sub-libraries-with-backport-dep ()
  (should (equal '() (package-lint-test--run
                      ";; Package-Requires: ((org \"9.3\"))\n(require 'ol)"))))

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
  (should-not
   (with-temp-buffer
     (insert ";; Version Control: 0\n")
     (package-lint-looks-like-a-package-p)))
  (should-not (with-temp-buffer (package-lint-looks-like-a-package-p)))
  (should-not
   (with-temp-buffer
     (insert ";; Dummy-Header: dummy-value\n")
     (package-lint-looks-like-a-package-p))))

(ert-deftest package-lint-test-error-unmatched-first-and-last-lines ()
  (should
   (member
    '(1 0 error "package.el cannot parse this buffer: Search failed: \";;; test.el ends here\"")
    (package-lint-test--run "" :footer "\n\n;;; Test.el ends here\n"))))

(ert-deftest package-lint-test-error-missing-provide-form ()
  (should
   (equal
    '((1 0 error "There is no (provide 'test) form."))
    (package-lint-test--run "" :provide ""))))

(ert-deftest package-lint-test-error-mismatched-provide-form ()
  (should
   (equal
    '((1 0 error "There is no (provide 'test) form."))
    (package-lint-test--run "" :provide "(provide 'blargh)"))))

(ert-deftest package-lint-test-accept-provide-me ()
  (should
   (equal '() (package-lint-test--run "" :provide "(provide-me)"))))

(ert-deftest package-lint-test-error-no-commentary ()
  (should
   (equal
    '((1 0 error "Package should have a ;;; Commentary section."))
    (package-lint-test--run "" :commentary "\n"))))

(ert-deftest package-lint-test-error-empty-commentary ()
  (should
   (equal
    '((4 0 error "Package should have a non-empty ;;; Commentary section."))
    (package-lint-test--run "" :commentary ";;; Commentary:\n ;;   \n \n\n;;; Code:\n"))))

(ert-deftest package-lint-test-accept-unprefixed-defadvice ()
  (should (equal '() (package-lint-test--run "(defadvice foo (before ignore))")))
  ;; Test if the special case we use for `defadvice' doesn't get
  ;; confused by weird spacing.
  (should (equal '() (package-lint-test--run "   (  defadvice \t\n\n foo (before ignore))"))))

(ert-deftest package-lint-test-accept-unprefixed-cl-defmethod ()
  (should (equal '() (package-lint-test--run ";; Package-Requires: ((emacs \"25.1\"))
\(cl-defmethod foo ()"))))

(ert-deftest package-lint-test-minor-mode-global-t ()
  (should
   (equal
    '((6 0 error "Global minor modes should be autoloaded or, rarely, `:require' their defining file (i.e. \":require 'test\"), to support the customization variable of the same name."))
    (package-lint-test--run "(define-minor-mode test-mode \"\" :global t)"))))

(ert-deftest package-lint-test-globalized-minor-mode ()
  ;; Check for missing :require.
  (should
   (equal
    '()
    (package-lint-test--run ";;;###autoload\n(define-globalized-minor-mode test-mode ignore ignore)")))
  ;; TODO
  ;; (should
  ;;  (equal
  ;;   '(7 0 warn "When autoloaded, global minor modes should have no `:require' form.")
  ;;   (package-lint-test--run ";;;###autoload\n(define-globalized-minor-mode test-mode ignore ignore :require 'test)")))
  (should
   (equal
    '((6 0 error "Global minor modes should be autoloaded or, rarely, `:require' their defining file (i.e. \":require 'test\"), to support the customization variable of the same name."))
    (package-lint-test--run "(define-globalized-minor-mode test-mode ignore ignore)")))
  ;; Check for incorrect :require.
  (should
   (equal
    '((6 0 error "Global minor modes should be autoloaded or, rarely, `:require' their defining file (i.e. \":require 'test\"), to support the customization variable of the same name."))
    (package-lint-test--run "(define-globalized-minor-mode test-mode ignore ignore :require 'blargh)"))))

(ert-deftest package-lint-test-warning-eval-after-load ()
  (should
   (equal
    '((6 1 warning "`eval-after-load' is for use in configurations, and should rarely be used in packages."))
    (package-lint-test--run "(eval-after-load 'foobar\n body)")))
  (should
   (equal
    '((7 1 warning "`with-eval-after-load' is for use in configurations, and should rarely be used in packages."))
    (package-lint-test--run ";; Package-Requires: ((emacs \"24.4\"))\n(with-eval-after-load 'foobar\n body)"))))

(ert-deftest package-lint-test-error-defgroup-name ()
  (should
   (equal
    '((6 0 error "Customization groups should not end in \"-mode\" unless that name would conflict with their parent group."))
    (package-lint-test--run "(defgroup test-mode nil \"\" :group 'programming)")))
  (should
   (equal
    '((6 0 error "Customization groups should not end in \"-mode\" unless that name would conflict with their parent group."))
    (package-lint-test--run "(defgroup test-mode nil \"\" :group 'testing)")))
  (should
   (equal
    '()
    (package-lint-test--run "(defgroup test-mode nil \"\" :group 'test)"))))

(ert-deftest package-lint-test-error-defgroup-no-parent ()
  (should
   (equal
    '((6 0 error "Customization groups should specify a parent via `:group'."))
    (package-lint-test--run "(defgroup test nil \"\")"))))

(ert-deftest package-lint-test-error-defalias-name ()
  (should
   (equal
    '((6 0 error "Aliases should start with the package's prefix \"test\"."))
    (package-lint-test--run "(defalias 'foobar 'string-equal)")))
  (should
   (equal
    '((6 0 error "Aliases should start with the package's prefix \"test\"."))
    (package-lint-test--run "(defvaralias 'foobar 'string-equal)")))
  (should
   (equal
    '()
    (package-lint-test--run "(defalias 'test-foobar 'string-equal)")))
  (should
   (equal
    '()
    (package-lint-test--run "(defvaralias 'test-foobar 'string-equal)"))))

(ert-deftest package-lint-test-error-format-string ()
  (should
   (equal
    '((6 0 error "You should depend on (emacs \"26.1\") if you need format field numbers."))
    (package-lint-test--run "(format \"%1$s\" \"foo\")")))
  (should
   (equal
    '()
    (package-lint-test--run "(format \"%s\" \"foo\")")))
  (should
   (equal
    '()
    (package-lint-test--run "'(error . 0)")))
  (should
   (equal
    '()
    (package-lint-test--run "(format \"%%1$s\" \"foo\")")))
  (should
   (equal
    '()
    (package-lint-test--run ";; Package-Requires: ((emacs \"26.1\"))
\(format \"%1$s\" \"foo\")"))))

(ert-deftest package-lint-test-accept-quasiquoted-defalias ()
  (should
   (equal
    '()
    (package-lint-test--run "(defalias ',foo 'bar)"))))

(ert-deftest package-lint-test-no-emacs-in-package-name ()
  (should
   (equal
    '((1 0 warning "The word \"emacs\" is redundant in Emacs package names."))
    (package-lint-test--run "" :featurename "emacs-package"))))

(ert-deftest package-lint-test-warn-about-lonely-parens ()
  (should
   (equal
    '((7 0 warning "Closing parens should not be wrapped onto new lines."))
    (package-lint-test--run "(hello\n)")))
  (should
   (equal
    '()
    (package-lint-test--run "(hello\n 'world)")))
  (should
   (equal
    '((7 5 warning "Closing parens should not be wrapped onto new lines."))
    (package-lint-test--run "(foo (hello\n     ) bar)")))
  (should
   (equal
    '((7 2 warning "Closing parens should not be wrapped onto new lines."))
    (package-lint-test--run "(hello\n  )    ; foo"))))

(ert-deftest package-lint-test-accept-lonely-parens-with-preceding-comment ()
  (should
   (equal
    '()
    (package-lint-test--run "(hello 'world\n ;; a comment\n)"))))

(ert-deftest package-lint-test-accept-valid-prefix-mappings ()
  (should
   (equal
    '()
    (package-lint-test--run "(defun org-foobar-test ()) (provide 'ox-foobar)" :provide "ox-foobar" :featurename "ox-foobar"))))

(ert-deftest package-lint-test-error-invalid-prefix-mappings ()
  (should
   (equal
    '((6 0 error "\"org-foobaz-test\" doesn't start with package's prefix \"ox-foobar\"."))
    (package-lint-test--run "(defun org-foobaz-test ()) (provide 'ox-foobar)" :provide "ox-foobar" :featurename "ox-foobar"))))

(ert-deftest package-lint-test-accept-provide-theme ()
  (should
   (equal
    '()
    (package-lint-test--run "(provide-theme 'foo)" :provide "" :featurename "foo-theme"))))

(ert-deftest package-lint-test-reject-mismatched-provide-theme ()
  (should
   (equal
    '((1 0 error "There is no (provide-theme 'foo) form."))
    (package-lint-test--run "(provide-theme 'bar)" :provide "" :featurename "foo-theme"))))

(provide 'package-lint-test)
;;; package-lint-test.el ends here
