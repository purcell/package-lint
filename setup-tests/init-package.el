;; init-package.el --- Init for package-lint linting/checks

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-check-signature nil)
(package-generate-autoloads "package-lint"
			    (file-name-parent-directory (file-name-directory load-file-name)))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(let ((needed-packages '(cl-lib let-alist compat)))
  (mapc (lambda (pkg)
	  (unless (package-installed-p pkg)
	    (package-install pkg)))
	needed-packages))

