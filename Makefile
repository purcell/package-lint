EMACS ?= emacs

# A space-separated list of required package names
DEPS = cl-lib let-alist compat

INIT_PACKAGES="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (dolist (pkg '(${DEPS})) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
	(package-refresh-contents)) \
      (package-install pkg))) \
  (unless package-archive-contents (package-refresh-contents)) \
  )"

EMACS_BATCH=${EMACS} -Q -batch --eval ${INIT_PACKAGES}

all: clean-elc compile package-lint test

package-lint:
	${EMACS} -Q --eval ${INIT_PACKAGES} -batch -l package-lint.el -f package-lint-batch-and-exit package-lint.el

compile: clean-elc
	${EMACS} -Q --eval ${INIT_PACKAGES} -L . -batch -f batch-byte-compile *.el

clean-elc:
	rm -f f.elc

TEST_SELECTOR ?= t
test-unit:
	@echo "---- Run unit tests"
	@${EMACS_BATCH} \
		 -l package-lint.el \
		 -l package-lint-test.el \
		 --eval "(ert-run-tests-batch-and-exit '${TEST_SELECTOR})" \
		 && echo "OK"

test-batch:
	@echo "---- Assert clean package passes batch linting"
	@${EMACS_BATCH} \
		 -l package-lint.el \
		 -f package-lint-batch-and-exit \
		 batch-tests/is-clean.el && echo "OK"
	@echo "---- Assert package with demoted warnings passes batch linting"
	@${EMACS_BATCH} \
		 --eval "(setq package-lint-batch-fail-on-warnings nil)" \
		 -l package-lint.el \
		 -f package-lint-batch-and-exit \
		 batch-tests/just-warnings.el && echo "OK"
	@echo "---- Assert package with warnings fails batch linting"
	@${EMACS_BATCH} \
		 -l package-lint.el \
		 -f package-lint-batch-and-exit \
		 batch-tests/just-warnings.el && \
	    exit 1 || echo "OK"
	@echo "---- Assert package with errors fails batch linting"
	@${EMACS_BATCH} \
		 -l package-lint.el \
		 -f package-lint-batch-and-exit \
		 batch-tests/has-errors.el && \
	    exit 1 || echo "OK"

test: test-unit test-batch

.PHONY:	all compile clean-elc package-lint test test-unit test-batch
