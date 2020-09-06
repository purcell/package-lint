#!/bin/sh -e

EMACS="${EMACS:=emacs}"

NEEDED_PACKAGES="cl-lib let-alist"

INIT_PACKAGE_EL="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (unless package-archive-contents \
     (package-refresh-contents)) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"

# Refresh package archives, because the test suite needs to see at least
# package-lint and cl-lib.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL"

# Byte compile, failing on byte compiler errors, or on warnings unless ignored
if [ -n "${EMACS_LINT_IGNORE+x}" ]; then
    ERROR_ON_WARN=nil
else
    ERROR_ON_WARN=t
fi

"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l package-lint.el \
         --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
         -f batch-byte-compile \
         package-lint.el package-lint-test.el
# Lint ourselves
# Lint failures are ignored if EMACS_LINT_IGNORE is defined, so that lint
# failures on Emacs 24.2 and below don't cause the tests to fail, as these
# versions have buggy imenu that reports (defvar foo) as a definition of foo.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -L . \
         --eval "(require 'package-lint)" \
         -f package-lint-batch-and-exit \
         package-lint.el package-lint-test.el || [ -n "${EMACS_LINT_IGNORE+x}" ]
# Finally, run the testsuite
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l package-lint.elc \
         -l package-lint-test.el \
         -f ert-run-tests-batch-and-exit

# Check for correct exit codes

echo "Assert clean package passes batch linting"
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l package-lint.el \
         -f package-lint-batch-and-exit \
         batch-tests/is-clean.el
"$EMACS" -Q -batch \
echo "Assert package with demoted warnings passes batch linting"
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         --eval "(setq package-lint-batch-fail-on-warnings nil)" \
         -l package-lint.el \
         -f package-lint-batch-and-exit \
         batch-tests/just-warnings.el
echo "Assert package with warnings fails batch linting"
if "$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l package-lint.el \
         -f package-lint-batch-and-exit \
         batch-tests/just-warnings.el; then
    echo "Didn't report failure when batch-linting file with errors"
    exit 1
fi
echo "Assert package with errors fails batch linting"
if "$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l package-lint.el \
         -f package-lint-batch-and-exit \
         batch-tests/has-errors.el; then
    echo "Didn't report failure when batch-linting file with errors"
    exit 1
fi
