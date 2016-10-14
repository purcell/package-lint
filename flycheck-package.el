;;; flycheck-package.el --- A Flycheck checker for elisp package authors

;; Copyright (C) 2014-2016  Steve Purcell, Fanael Linithien

;; Author: Steve Purcell <steve@sanityinc.com>
;;         Fanael Linithien <fanael4@gmail.com>
;; Keywords: lisp
;; Version: 0
;; Package-Requires: ((cl-lib "0.5") (flycheck "0.22") (emacs "24"))

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

;; Checks will currently be enabled only if a "Package-Requires:" or
;; "Package-Version:" header is present in the file.

;;; Code:

(eval-when-compile (require 'pcase))    ; `pcase-dolist' is not autoloaded
(eval-when-compile (require 'cl-lib))
(require 'flycheck)
(require 'package)
(require 'lisp-mnt)
(require 'finder)
(require 'imenu)


;;; Compatibility

(defalias 'flycheck-package--package-desc-summary
  (if (fboundp 'package-desc-summary)
      'package-desc-summary
    'package-desc-doc))


;;; Machinery

(defun flycheck-package--start (checker callback)
  "Flycheck start function for CHECKER, invoking CALLBACK."
  (funcall callback
           'finished
           (mapcar (lambda (x)
                     (apply #'flycheck-error-new-at `(,@x :checker ,checker)))
                   (condition-case err
                       (flycheck-package--check-all)
                     (error
                      (funcall callback 'errored (error-message-string err))
                      (signal (car err) (cdr err)))))))

(defvar flycheck-package--errors nil
  "List of errors and warnings for the current buffer.
This is bound dynamically while the checks run.")

(defconst flycheck-package--functions-and-macros-added-alist
  (eval-when-compile
    (list
     (cons '(24)
           (regexp-opt
            '("bidi-string-mark-left-to-right" "condition-case-unless-debug"
              "current-bidi-paragraph-direction" "file-selinux-context" "letrec"
              "make-composed-keymap" "read-char-choice" "run-hook-wrapped"
              "set-file-selinux-context" "server-eval-at" "special-variable-p"
              "string-prefix-p" "url-queue-retrieve" "window-body-height"
              "window-stage-get" "window-stage-put" "window-total-width"
              "window-valid-p" "with-wrapper-hook")))
     (cons '(24 3)
           (regexp-opt
            '("autoloadp" "autoload-do-load" "buffer-narrowed-p" "defvar-local"
              "file-name-base" "function-get" "posnp" "setq-local"
              "system-groups" "system-users" "url-encode-url"
              "with-temp-buffer-window")))
     (cons '(24 4)
           (regexp-opt
            '("add-face-text-property" "cl-tagbody"
              "completion-table-with-cache" "completion-table-merge"
              "define-alternative" "define-error"
              "display-monitor-attributes-list" "file-acl"
              "file-extended-attributes" "fill-single-char-nobreak-p"
              "frame-monitor-attributes" "group-gid" "group-real-gid"
              "get-pos-property" "macrop" "set-file-acl" "special-form-p"
              "string-suffix-p" "window-bottom-divider-width"
              "window-header-line-height" "window-mode-line-height"
              "window-right-divider-width" "window-scroll-bar-width"
              "window-text-pixel-size" "with-eval-after-load"
              "zlib-decompress-region")))
     (cons '(25)
           (regexp-opt
            '("alist-get" "backward-word-strictly"
              "bidi-find-overridden-directionality"
              "buffer-substring-with-bidi-context" "bufferpos-to-filepos"
              "checkdoc-file" "char-fold-to-regexp" "cl-digit-char-p"
              "cl-fresh-line" "cl-parse-integer" "default-font-width"
              "define-advice" "define-inline" "directory-name-p"
              "directory-files-recursively" "file-notify-valid-p"
              "filepos-to-bufferpos" "forward-word-strictly" "format-message"
              "frame-edges" "frame-geometry" "frame-scroll-bar-height"
              "funcall-interactively" "function-put"
              "horizontal-scroll-bars-available-p" "if-let" "macroexpand-1"
              "make-process" "mouse-absolute-pixel-position" "set-binary-mode"
              "set-mouse-absolute-pixel-position" "string-collate-equalp"
              "string-collate-lessp" "string-greaterp" "thread-first"
              "thread-last" "toggle-horizontal-scroll-bar" "when-let"
              "window-absolute-pixel-position" "window-font-height"
              "window-font-width" "window-max-chars-per-line"
              "window-preserve-size" "window-scroll-bar-height"
              "with-file-modes")))))
  "An alist of function/macro names and when they were added to Emacs.")

(defun flycheck-package--check-all ()
  "Return a list of errors/warnings for the current buffer."
  (let ((flycheck-package--errors '()))
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          (when (flycheck-package--looks-like-a-package)
            (flycheck-package--check-keywords-list)
            (flycheck-package--check-package-version-present)
            (flycheck-package--check-lexical-binding-is-on-first-line)
            (let ((desc (flycheck-package--check-package-el-can-parse)))
              (when desc
                (flycheck-package--check-package-summary desc)))
            (let ((deps (flycheck-package--check-dependency-list)))
              (flycheck-package--check-lexical-binding-requires-emacs-24 deps)
              (flycheck-package--check-macros-functions-available-in-emacs deps))
            (let ((definitions (flycheck-package--get-defs)))
              (flycheck-package--check-defs-prefix definitions)
              (flycheck-package--check-symbol-separators definitions))))))
    flycheck-package--errors))

(defun flycheck-package--error (line col type message)
  "Construct a datum for error at LINE and COL with TYPE and MESSAGE."
  (push (list line col type message) flycheck-package--errors))


;;; Checks

(defun flycheck-package--check-keywords-list ()
  "Verify that package keywords are listed in `finder-known-keywords'."
  (when (flycheck-package--goto-header "Keywords")
    (let ((line-no (line-number-at-pos))
          (keywords (lm-keywords-list)))
      (dolist (keyword keywords)
        (unless (assoc (intern keyword) finder-known-keywords)
          (flycheck-package--error
           line-no 1 'warning
           (format "\"%s\" is not a standard package keyword: see `finder-known-keywords'." keyword)))))))

(defun flycheck-package--check-dependency-list ()
  "Check the contents of the \"Package-Requires\" header.
Return a list of well-formed dependencies, same as
`flycheck-package--check-well-formed-dependencies'."
  (when (flycheck-package--goto-header "Package-Requires")
    (let ((position (match-beginning 3))
          (line-no (line-number-at-pos))
          (deps (match-string 3)))
      (condition-case err
          (pcase-let ((`(,parsed-deps . ,parse-end-pos) (read-from-string deps)))
            (unless (= parse-end-pos (length deps))
              (flycheck-package--error
               line-no 1 'error
               "More than one expression provided."))
            (let ((deps (flycheck-package--check-well-formed-dependencies position line-no parsed-deps)))
              (flycheck-package--check-packages-installable deps)
              (flycheck-package--check-deps-use-non-snapshot-version deps)
              (flycheck-package--check-deps-do-not-use-zero-versions deps)
              (flycheck-package--check-do-not-depend-on-cl-lib-1.0 deps)
              deps))
        (error
         (flycheck-package--error
          line-no 1 'error
          (format "Couldn't parse \"Package-Requires\" header: %s" (error-message-string err)))
         nil)))))

(defun flycheck-package--check-well-formed-dependencies (position line-no parsed-deps)
  "Check that dependencies listed at POSITION on LINE-NO are well-formed.
These PARSED-DEPS must have the format (name \"version\").
Return a list of well-formed dependencies, where each element is of
the form (PACKAGE-NAME PACKAGE-VERSION LINE-NO LINE-BEGINNING-OFFSET)."
  (let (valid-deps)
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
                         (format "( *\\(%s\\)\\(?:)\\|[^[:alnum:]_\\-].*?)\\)"
                                 (regexp-quote (symbol-name package-name)))))
                    (if (re-search-forward pattern (line-end-position) t)
                        (- (1+ (match-beginning 1)) line-start)
                      1)))))
           (if (ignore-errors (version-to-list package-version))
               (push (list package-name
                           (version-to-list package-version)
                           line-no
                           offset)
                     valid-deps)
             (flycheck-package--error
              line-no offset 'error
              (format "%S is not a valid version string: see `version-to-list'."
                      package-version)))))
        (_
         (flycheck-package--error
          line-no 1 'error
          (format "Expected (package-name \"version-num\"), but found %S." entry)))))
    valid-deps))

(defun flycheck-package--check-packages-installable (valid-deps)
  "Check that all VALID-DEPS are available for installation."
  (pcase-dolist (`(,package-name ,package-version ,line-no ,offset) valid-deps)
    (if (eq 'emacs package-name)
        (unless (version-list-<= '(24) package-version)
          (flycheck-package--error
           line-no offset 'error
           "You can only depend on Emacs version 24 or greater."))
      ;; Not 'emacs
      (let ((archive-entry (assq package-name package-archive-contents)))
        (if archive-entry
            (let ((best-version (flycheck-package--lowest-installable-version-of package-name)))
              (when (version-list-< best-version package-version)
                (flycheck-package--error
                 line-no offset 'warning
                 (format "Version dependency for %s appears too high: try %s" package-name
                         (package-version-join best-version)))))
          (flycheck-package--error
           line-no offset 'error
           (format "Package %S is not installable." package-name)))))))

(defun flycheck-package--check-deps-use-non-snapshot-version (valid-deps)
  "Warn about any VALID-DEPS on snapshot versions of packages."
  (pcase-dolist (`(,package-name ,package-version ,line-no ,offset) valid-deps)
    (unless (version-list-< package-version '(19001201 1))
      (flycheck-package--error
       line-no offset 'warning
       (format "Use a non-snapshot version number for dependency on \"%S\" if possible."
               package-name)))))

(defun flycheck-package--check-deps-do-not-use-zero-versions (valid-deps)
  "Warn about VALID-DEPS on \"0\" versions of packages."
  (pcase-dolist (`(,package-name ,package-version ,line-no ,offset) valid-deps)
    (when (equal package-version '(0))
      (flycheck-package--error
       line-no offset 'warning
       (format "Use a properly versioned dependency on \"%S\" if possible."
               package-name)))))

(defun flycheck-package--check-lexical-binding-requires-emacs-24 (valid-deps)
  "Warn about use of `lexical-binding' when Emacs 24 is not among VALID-DEPS."
  (goto-char (point-min))
  (when (flycheck-package--lexical-binding-declared-in-header-line-p)
    (let* ((lexbind-line (line-number-at-pos))
           (lexbind-col (1+ (- (match-beginning 1) (line-beginning-position)))))
      (unless (assq 'emacs valid-deps)
        (flycheck-package--error
         lexbind-line lexbind-col 'warning
         "You should depend on (emacs \"24\") if you need lexical-binding.")))))

(defun flycheck-package--check-macros-functions-available-in-emacs (valid-deps)
  "Warn about use of functions/macros that are not available in the Emacs version in VALID-DEPS."
  (let ((emacs-version-dep (or (cadr (assq 'emacs valid-deps)) '(0))))
    (pcase-dolist (`(,added-in-version . ,regexp) flycheck-package--functions-and-macros-added-alist)
      (when (version-list-< emacs-version-dep added-in-version)
        (goto-char (point-min))
        (while (re-search-forward (concat "(\\s-*?\\(" regexp "\\)\\_>") nil t)
          (unless (let ((ppss (syntax-ppss)))
                    (or (nth 3 ppss) (nth 4 ppss)))
            (flycheck-package--error
             (line-number-at-pos)
             (current-column)
             'warning
             (format "You should depend on (emacs \"%s\") if you need `%s'."
                     (mapconcat #'number-to-string added-in-version ".")
                     (match-string-no-properties 1)))))))))

(defun flycheck-package--check-lexical-binding-is-on-first-line ()
  "Check that any `lexical-binding' declaration is on the first line of the file."
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
             (flycheck-package--error 1 1 'error (error-message-string err))
             (cl-return-from return nil)))
          (when (or lexical-binding-found-at-end
                    ;; In case this is an Emacs from before `hack-local-variables'
                    ;; started to warn about `lexical-binding' on a line other
                    ;; than the first.
                    (and (cdr (assq 'lexical-binding file-local-variables-alist))
                         (not (flycheck-package--lexical-binding-declared-in-header-line-p))))
            (flycheck-package--error
             1 1 'error
             "`lexical-binding' must be set in the first line.")))))))

(defun flycheck-package--check-do-not-depend-on-cl-lib-1.0 (valid-deps)
  "Check that any dependency on \"cl-lib\" is on a remotely-installable version."
  (let ((cl-lib-dep (assq 'cl-lib valid-deps)))
    (when cl-lib-dep
      (let ((cl-lib-version (nth 1 cl-lib-dep)))
        (when (version-list-<= '(1) cl-lib-version)
          (flycheck-package--error
           (nth 2 cl-lib-dep) (nth 3 cl-lib-dep) 'error
           (format "Depend on the latest 0.x version of cl-lib rather than on version \"%S\".
Alternatively, depend on (emacs \"24.3\") or greater, in which cl-lib is bundled."
                   cl-lib-version)))))))

(defun flycheck-package--check-package-version-present ()
  "Check that a valid \"Version\" header is present."
  (let ((version (flycheck-package--goto-header (rx (? "Package-") "Version"))))
    (if version
        (unless (ignore-errors (version-to-list version))
          (flycheck-package--error
           (line-number-at-pos)
           (1+ (- (match-beginning 3) (line-beginning-position)))
           'warning
           (format "\"%s\" is not a valid version. MELPA will handle this, but other archives will not." version)))
      (flycheck-package--error
       1 1 'warning
       "\"Version:\" or \"Package-Version:\" header is missing. MELPA will handle this, but other archives will not."))))

(defun flycheck-package--check-package-el-can-parse ()
  "Check that `package-buffer-info' can read metadata from this file.
If it can, return the read metadata."
  (condition-case err
      (let ((orig-buffer (current-buffer)))
        ;; We've reported version header issues separately, so rule them out here
        (with-temp-buffer
          (insert-buffer-substring-no-properties orig-buffer)
          (flycheck-package--update-or-insert-version "0")
          (package-buffer-info)))
    (error
     (flycheck-package--error
      1 1
      'error
      (format "package.el cannot parse this buffer: %s" (error-message-string err)))
     nil)))

(defun flycheck-package--check-package-summary (desc)
  "Check the summary for package with descriptor DESC.
DESC is a struct as returned by `package-buffer-info'."
  (let ((summary (flycheck-package--package-desc-summary desc)))
    (cond
     ((string-empty-p summary)
      (flycheck-package--error
       1 1
       'warning
       "Package should have a non-empty summary."))
     ((> (length summary) 50)
      (flycheck-package--error
       1 1
       'warning
       "The package summary is too long. It should be at most 50 characters.")))
    (when (save-match-data
            (let ((case-fold-search t))
              (and (string-match "\\<emacs\\>" summary)
                   (not (string-match-p "[[:space:]]+lisp" summary (match-end 0))))))
      (flycheck-package--error
       1 1
       'warning
       "Including \"Emacs\" in the package description is usually redundant."))))

(defun flycheck-package--check-symbol-separators (definitions)
  "Check that symbol DEFINITIONS don't contain non-standard separators."
  (pcase-dolist (`(,name . ,position) definitions)
    (when (string-match "[:/]" name)
      (let ((match-pos (match-beginning 0)))
        ;; As a special case, allow `/=' when at the end of a symbol.
        (when (or (not (string-match (rx "/=" string-end) name))
                  (/= match-pos (match-beginning 0)))
          (goto-char position)
          (flycheck-package--error
           (line-number-at-pos) 1 'error
           (format "`%s' contains a non-standard separator `%s', use hyphens instead."
                   name (substring-no-properties name match-pos (1+ match-pos)))))))))

(defun flycheck-package--check-defs-prefix (definitions)
  "Verify that symbol DEFINITIONS start with package prefix."
  (let* ((prefix (flycheck-package--get-package-prefix))
         (prefix-re (rx-to-string `(seq string-start ,prefix (or "-" string-end)))))
    (when prefix
      (pcase-dolist (`(,name . ,position) definitions)
        (unless (string-match-p prefix-re name)
          (let ((line-no (line-number-at-pos position)))
            (flycheck-package--error
             line-no 1 'error
             (format "\"%s\" doesn't start with package's prefix \"%s\"."
                     name prefix))))))))


;;; Helpers

(defun flycheck-package--looks-like-a-package ()
  "Return non-nil if this buffer appears to be intended as a package."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (concat lm-header-prefix (rx (or "Package-Version" "Package-Requires")))
     nil t)))

(defun flycheck-package--lowest-installable-version-of (package)
  "Return the lowest version of PACKAGE available for installation."
  (let ((descriptors (cdr (assq package package-archive-contents))))
    (if (fboundp 'package-desc-version)
        (car (sort (mapcar 'package-desc-version descriptors)
                   #'version-list-<))
      (aref descriptors 0))))

(defun flycheck-package--goto-header (header-name)
  "Move to the first occurrence of HEADER-NAME in the file.
If the return value is non-nil, then point will be at the end of
the file, and the second and third match groups will contain the name and
value of the header with any leading or trailing whitespace removed."
  (let ((initial-point (point)))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward (concat (lm-get-header-re header-name) "\\(.*?\\) *$") nil t)
          (match-string-no-properties 3)
        (goto-char initial-point)
        nil))))

(defun flycheck-package--update-or-insert-version (version)
  "Ensure current buffer has a \"Version: VERSION\" header."
  (if (flycheck-package--goto-header "Version")
      (move-beginning-of-line nil)
    (forward-line))
  (insert (format ";; Version: %s" version))
  (newline))

(defun flycheck-package--get-header-line-file-local-variables ()
  "Return local variables specified in the -*- line.
Returns an alist of elements (VAR . VAL), where VAR is a variable
and VAL is the specified value.

For details, see `hack-local-variables-prop-line'."
  (cl-letf (((symbol-function #'message) #'ignore))
    (hack-local-variables-prop-line)))

(defun flycheck-package--lexical-binding-declared-in-header-line-p ()
  "Test if `lexical-binding' is declared in the -*- line."
  ;; Test the `cdr' to see if it's actually true, because
  ;; -*- lexical-binding: nil -*-
  ;; is legal, if silly.
  (cdr (assq 'lexical-binding (flycheck-package--get-header-line-file-local-variables))))

(defun flycheck-package--get-defs ()
  "Return a list of all variables and functions defined in the current buffer.

The returned list is of the form (SYMBOL-NAME . POSITION)."
  ;; We probably could use Semantic instead, but it's a *global* minor mode and
  ;; it tends to be quite heavy, so use Imenu instead; if the user has Semantic
  ;; enabled, Imenu will use its index anyway.
  (save-excursion
    (let ((result '())
          (index
           ;; In case it's actually Semantic, tell it not to decorate symbol
           ;; names.
           (let ((semantic-imenu-summary-function 'semantic-format-tag-name))
             (funcall imenu-create-index-function))))
      (dolist (entry index)
        (pcase entry
          ((and `(,submenu-name . ,submenu-elements)
                (guard (consp submenu-elements)))
           (when (member submenu-name '("Variables" "Defuns"))
             (setq result (nconc (reverse submenu-elements) result))))
          (_
           (push entry result))))
      ;; If it's Semantic, then it returns overlays, not positions. Convert
      ;; them.
      (dolist (entry result)
        (when (overlayp (cdr entry))
          (setcdr entry (overlay-start (cdr entry)))))
      (nreverse result))))

(defun flycheck-package--get-package-prefix ()
  "Return package prefix string (i.e. the symbol the package `provide's).
Prefix is returned without any `-mode' suffix."
  (goto-char (point-max))
  (when (re-search-backward (rx "(provide '" (group (1+ (or (syntax word) (syntax symbol))))) nil t)
    (replace-regexp-in-string "-mode\\'" "" (match-string-no-properties 1))))


;;; Checker definition

(flycheck-define-generic-checker 'emacs-lisp-package
  "A checker for \"Package-Requires\" headers."
  :start #'flycheck-package--start
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
