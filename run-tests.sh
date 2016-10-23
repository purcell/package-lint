#!/bin/sh -e

EMACS="${EMACS:=emacs}"

INIT_PACKAGE_EL="(progn
  (require 'package)
  (push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives)
  (package-initialize))"

# Refresh package archives, because the test suite needs to see at least
# package-lint and cl-lib.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         --eval '(package-refresh-contents)' \
         --eval "(package-install 'cl-lib)"
# Byte compile, failing on byte compiler warnings or errors
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l package-lint.el \
         --eval '(setq byte-compile-error-on-warn t)' \
         -f batch-byte-compile \
         package-lint.el package-lint-test.el
# Lint ourselves
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l package-lint.el \
         -f package-lint-batch-and-exit \
         package-lint.el package-lint-test.el
# Finally, run the testsuite
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l package-lint.el \
         -l package-lint-test.el \
         -f ert-run-tests-batch-and-exit
