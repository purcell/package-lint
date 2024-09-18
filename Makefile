SHELL = sh
.SHELLFLAGS = -e
EMACS ?= emacs
CURRENT_DIR = ${CURDIR}
BATCH_TESTS = ${CURRENT_DIR}/batch-tests
SETUP_TESTS = ${CURRENT_DIR}/setup-tests
CHECK_FILES = package-lint.el package-lint-test.el
INIT_PACKAGE_EL = ${SETUP_TESTS}/init-package.el
BASIC_INIT_EL = ${SETUP_TESTS}/init.el
PACKAGE_LINT_TEST = ${CURRENT_DIR}/package-lint-test.el
PACKAGE_LINT = ${CURRENT_DIR}/package-lint.el
PACKAGE_LINT_COMPILED = ${CURRENT_DIR}/package-lint.elc
PACKAGE_LINT_AUTOLOAD = ${CURRENT_DIR}/package-lint-autoloads.el
SELECTOR ?= t
EMACS_LINT_IGNORE ?=
BATCH_MODE = ${EMACS} -Q -batch
BYTE_COMPILE = "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})"
ERR_MESSAGE = "\n<<<------------ Didn't report failure when batch-linting file with errors ------------>>>\n"

# Byte compile, failing on byte compiler errors, or on warnings unless ignored
ifeq ($(EMACS_LINT_IGNORE),t)
    ERROR_ON_WARN = nil
else
    ERROR_ON_WARN = t 
endif

define LOAD_PACKAGE_LINT
-l ${PACKAGE_LINT_COMPILED} \
-l ${PACKAGE_LINT_TEST}
endef

define CHECK_PACKAGE
-l ${PACKAGE_LINT} \
-f package-lint-batch-and-exit
endef

define EMACS_WITH_PACKAGE_ARCHIVES
${BATCH_MODE} \
-l ${INIT_PACKAGE_EL}
endef

define CHECK_EMACS
${EMACS_WITH_PACKAGE_ARCHIVES} \
${CHECK_PACKAGE}
endef

.PHONY: all test init lint test check run clean

all: ## run init, lint, test and check
all: init lint test check
	make clean

help: ## Display this help message.
	@printf 'Summary of available Makefile recipes:\n\n'
	@grep '##' Makefile | grep -v grep | column -t -s '##'
	@echo
init: ## Refresh package archives and byte-compile files
init: clean
	${EMACS_WITH_PACKAGE_ARCHIVES} \
	--eval ${BYTE_COMPILE} \
	-f batch-byte-compile \
	${CHECK_FILES}

clean: ## Clean up all temporary files created during testing.	
	find . -name "*.elc" -type f -delete
	rm -f ${PACKAGE_LINT_AUTOLOAD}

run: ## Open a clean emacs with a working version of package-lint loaded.
	${EMACS} -Q \
	-l ${BASIC_INIT_EL} \
	--file=${BATCH_TESTS}/has-errors.el

lint: # Lint failures are ignored if EMACS_LINT_IGNORE is defined, so that lint
# failures on Emacs 24.2 and below don't cause the tests to fail, as these
# versions have buggy imenu that reports (defvar foo) as a definition of foo.
	${EMACS_WITH_PACKAGE_ARCHIVES} \
	-L . \
	--eval "(require 'package-lint)" \
	-f package-lint-batch-and-exit \
	${CHECK_FILES} || [ -n "${EMACS_LINT_IGNORE}" ]

test: ## Run all ERT tests (set SELECTOR to specify only one).
test: init
	${EMACS_WITH_PACKAGE_ARCHIVES} \
	${LOAD_PACKAGE_LINT} \
	--eval "(ert-run-tests-batch-and-exit '${SELECTOR})"

check: ## Run the linter on test file
check: init
	@printf "\n<<<------------ Assert clean package passes batch linting ------------>>>\n"
	${CHECK_EMACS} ${BATCH_TESTS}/is-clean.el && echo "\n<<<------------ PASS ------------>>>\n"
	@printf "\n<<<------------ Assert package with demoted warnings passes batch linting ------------>>>\n"
	${EMACS_WITH_PACKAGE_ARCHIVES} \
	--eval "(setq package-lint-batch-fail-on-warnings nil)" \
	${PACKAGE_LINT} ${BATCH_TESTS}/just-warnings.el && echo "\n<<<------------ PASS ------------>>>\n"
	@printf "\n<<<------------ Assert package with warnings fails batch linting ------------>>>\n"
	${CHECK_EMACS} ${BATCH_TESTS}/just-warnings.el \
	&& echo ${ERR_MESSAGE} && exit 1 || echo "\n<<<------------ PASS ------------>>>\n"
	@printf "\n<<<------------ Assert package with errors fails batch linting ------------>>>\n"
	${CHECK_EMACS} ${BATCH_TESTS}/has-errors.el \
	&& echo ${ERR_MESSAGE} && exit 1 || echo "\n<<<------------ PASS ------------>>>\n"
