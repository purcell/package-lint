;; init.el --- Manual testing setup for package-lint

(require 'package)
(package-generate-autoloads "package-lint" ".")
(load (file-name-concat (file-name-parent-directory (file-name-directory load-file-name)) "package-lint-autoloads.el"))

;; Set up misc. Emacs config
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(toggle-debug-on-error)
