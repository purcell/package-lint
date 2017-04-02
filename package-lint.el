;;; package-lint.el --- A linting library for elisp package authors -*- lexical-binding: t -*-

;; Copyright (C) 2014-2017  Steve Purcell, Fanael Linithien

;; Author: Steve Purcell <steve@sanityinc.com>
;;         Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/purcell/package-lint
;; Keywords: lisp
;; Version: 0
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))

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

;; Provides a list of issues with the package metadata of a file,
;; e.g. the package dependencies it requires.

;; See function `package-lint-buffer'.

;; Checks will currently be enabled only if a "Package-Requires:" or
;; "Package-Version:" header is present in the file.

;;; Code:

(eval-when-compile (require 'pcase))    ; `pcase-dolist' is not autoloaded
(require 'cl-lib)
(require 'package)
(require 'lisp-mnt)
(require 'finder)
(require 'imenu)


;;; Compatibility

(defalias 'package-lint--package-desc-summary
  (if (fboundp 'package-desc-summary)
      'package-desc-summary
    'package-desc-doc))


;;; Machinery

(defvar package-lint--errors nil
  "List of errors and warnings for the current buffer.
This is bound dynamically while the checks run.")

(defmacro package-lint--match-symbols (&rest symbols)
  "Return a regexp matching the string names of all given SYMBOLS."
  (regexp-opt (mapcar #'symbol-name symbols)))

(defconst package-lint--libraries-added-alist
  (list
   (cons '(24 4)
         (package-lint--match-symbols
          nadvice
          subr-x))
   (cons '(25 1)
         (package-lint--match-symbols
          cl-generic
          js-jsx-mode
          map
          pinentry
          thunk)))
  "An alist of library names and when they were added to Emacs.")

(defconst package-lint--functions-and-macros-added-alist
  (list
   (cons '(24)
         (package-lint--match-symbols
          bidi-string-mark-left-to-right
          condition-case-unless-debug
          current-bidi-paragraph-direction
          file-selinux-context
          letrec
          make-composed-keymap
          pcase
          pcase-dolist
          pcase-let
          pcase-let*
          read-char-choice
          run-hook-wrapped
          server-eval-at
          set-file-selinux-context
          special-variable-p
          string-prefix-p
          url-queue-retrieve
          window-body-height
          window-stage-get
          window-stage-put
          window-total-width
          window-valid-p
          with-wrapper-hook))
   (cons '(24 3)
         (package-lint--match-symbols
          autoload-do-load
          autoloadp
          buffer-narrowed-p
          defvar-local
          file-name-base
          function-get
          posnp
          read-only-mode
          setq-local
          system-groups
          system-users
          tty-top-frame
          url-encode-url
          user-error
          with-temp-buffer-window))
   (cons '(24 4)
         (package-lint--match-symbols
          add-face-text-property
          add-function
          advice-add
          advice-remove
          cl-tagbody
          completion-table-merge
          completion-table-with-cache
          define-alternative
          define-error
          display-monitor-attributes-list
          file-acl
          file-extended-attributes
          fill-single-char-nobreak-p
          frame-monitor-attributes
          get-pos-property
          group-gid
          group-real-gid
          hash-table-keys
          hash-table-values
          macrop
          remove-function
          set-file-acl
          special-form-p
          string-blank-p
          string-empty-p
          string-join
          string-remove-prefix
          string-remove-suffix
          string-reverse
          string-suffix-p
          string-trim
          string-trim-left
          string-trim-right
          window-bottom-divider-width
          window-header-line-height
          window-mode-line-height
          window-right-divider-width
          window-scroll-bar-width
          window-text-pixel-size
          with-eval-after-load
          zlib-decompress-region))
   (cons '(25)
         (package-lint--match-symbols
          alist-get
          backward-word-strictly
          bidi-find-overridden-directionality
          buffer-substring-with-bidi-context
          bufferpos-to-filepos
          char-fold-to-regexp
          checkdoc-file
          cl-digit-char-p
          cl-fresh-line
          cl-parse-integer
          default-font-width
          define-advice
          define-inline
          directory-files-recursively
          directory-name-p
          file-notify-valid-p
          filepos-to-bufferpos
          format-message
          forward-word-strictly
          frame-edges
          frame-geometry
          frame-scroll-bar-height
          funcall-interactively
          function-put
          horizontal-scroll-bars-available-p
          if-let
          macroexpand-1
          make-process
          mouse-absolute-pixel-position
          pcase-defmacro
          pcase-exhaustive
          pcase-lambda
          set-binary-mode
          set-mouse-absolute-pixel-position
          string-collate-equalp
          string-collate-lessp
          string-greaterp
          thread-first
          thread-last
          toggle-horizontal-scroll-bar
          when-let
          window-absolute-pixel-position
          window-font-height
          window-font-width
          window-max-chars-per-line
          window-preserve-size
          window-scroll-bar-height
          with-displayed-buffer-window
          with-file-modes)))
  "An alist of function/macro names and when they were added to Emacs.")

(defconst package-lint--sane-prefixes
  (rx
   string-start
   (or
    "org-dblock-write:"
    "org-babel-execute:"
    "org-babel-default-header-args:"))
  "A regexp matching whitelisted non-standard symbol prefixes.")

(defun package-lint--check-all ()
  "Return a list of errors/warnings for the current buffer."
  (let ((package-lint--errors '()))
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          (package-lint--check-keywords-list)
          (package-lint--check-package-version-present)
          (package-lint--check-lexical-binding-is-on-first-line)
          (let ((desc (package-lint--check-package-el-can-parse)))
            (when desc
              (package-lint--check-package-summary desc)))
          (let ((deps (package-lint--check-dependency-list)))
            (package-lint--check-lexical-binding-requires-emacs-24 deps)
            (package-lint--check-libraries-available-in-emacs deps)
            (package-lint--check-macros-functions-available-in-emacs deps))
          (package-lint--check-for-literal-emacs-path)
          (let ((definitions (package-lint--get-defs)))
            (package-lint--check-defs-prefix definitions)
            (package-lint--check-symbol-separators definitions)))))
    package-lint--errors))

(defun package-lint--error (line col type message)
  "Construct a datum for error at LINE and COL with TYPE and MESSAGE."
  (push (list line col type message) package-lint--errors))


;;; Checks

(defun package-lint--check-for-literal-emacs-path ()
  "Verify package does not refer to \"\.emacs\.d\" literally.
Instead it should use `user-emacs-directory' or `locate-user-emacs-file'."
  (goto-char (point-min))
  (while (re-search-forward (rx (syntax string-quote) (0+ (not (syntax string-quote))) (or "/" "\\") ".emacs.d") nil t)
    (package-lint--error
     (line-number-at-pos) (current-column) 'warning
     "Use variable `user-emacs-directory' or function `locate-user-emacs-file' instead of a literal path to the Emacs user directory or files.")))

(defun package-lint--check-keywords-list ()
  "Verify that package keywords are listed in `finder-known-keywords'."
  (when (package-lint--goto-header "Keywords")
    (let ((line-no (line-number-at-pos))
          (keywords (lm-keywords-list)))
      (unless (cl-some (lambda (keyword) (assoc (intern keyword) finder-known-keywords)) keywords)
        (package-lint--error
         line-no 1 'warning
         (format "You should include standard keywords: see the variable `finder-known-keywords'."))))))

(defun package-lint--check-dependency-list ()
  "Check the contents of the \"Package-Requires\" header.
Return a list of well-formed dependencies, same as
`package-lint--check-well-formed-dependencies'."
  (when (package-lint--goto-header "Package-Requires")
    (let ((position (match-beginning 3))
          (line-no (line-number-at-pos))
          (deps (match-string 3)))
      (condition-case err
          (pcase-let ((`(,parsed-deps . ,parse-end-pos) (read-from-string deps)))
            (unless (= parse-end-pos (length deps))
              (package-lint--error
               line-no 1 'error
               "More than one expression provided."))
            (let ((deps (package-lint--check-well-formed-dependencies position line-no parsed-deps)))
              (package-lint--check-packages-installable deps)
              (package-lint--check-deps-use-non-snapshot-version deps)
              (package-lint--check-deps-do-not-use-zero-versions deps)
              (package-lint--check-do-not-depend-on-cl-lib-1.0 deps)
              deps))
        (error
         (package-lint--error
          line-no 1 'error
          (format "Couldn't parse \"Package-Requires\" header: %s" (error-message-string err)))
         nil)))))

(defun package-lint--check-well-formed-dependencies (position line-no parsed-deps)
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
             (package-lint--error
              line-no offset 'error
              (format "%S is not a valid version string: see `version-to-list'."
                      package-version)))))
        (_
         (package-lint--error
          line-no 1 'error
          (format "Expected (package-name \"version-num\"), but found %S." entry)))))
    valid-deps))

(defun package-lint--check-packages-installable (valid-deps)
  "Check that all VALID-DEPS are available for installation."
  (pcase-dolist (`(,package-name ,package-version ,line-no ,offset) valid-deps)
    (if (eq 'emacs package-name)
        (unless (version-list-<= '(24) package-version)
          (package-lint--error
           line-no offset 'error
           "You can only depend on Emacs version 24 or greater: package.el for Emacs 23 does not support the \"emacs\" pseudopackage."))
      ;; Not 'emacs
      (let ((archive-entry (assq package-name package-archive-contents)))
        (if archive-entry
            (let ((best-version (package-lint--lowest-installable-version-of package-name)))
              (when (version-list-< best-version package-version)
                (package-lint--error
                 line-no offset 'warning
                 (format "Version dependency for %s appears too high: try %s" package-name
                         (package-version-join best-version)))))
          (package-lint--error
           line-no offset 'error
           (format "Package %S is not installable." package-name)))))))

(defun package-lint--check-deps-use-non-snapshot-version (valid-deps)
  "Warn about any VALID-DEPS on snapshot versions of packages."
  (pcase-dolist (`(,package-name ,package-version ,line-no ,offset) valid-deps)
    (unless (version-list-< package-version '(19001201 1))
      (package-lint--error
       line-no offset 'warning
       (format "Use a non-snapshot version number for dependency on \"%S\" if possible."
               package-name)))))

(defun package-lint--check-deps-do-not-use-zero-versions (valid-deps)
  "Warn about VALID-DEPS on \"0\" versions of packages."
  (pcase-dolist (`(,package-name ,package-version ,line-no ,offset) valid-deps)
    (when (equal package-version '(0))
      (package-lint--error
       line-no offset 'warning
       (format "Use a properly versioned dependency on \"%S\" if possible."
               package-name)))))

(defun package-lint--check-lexical-binding-requires-emacs-24 (valid-deps)
  "Warn about use of `lexical-binding' when Emacs 24 is not among VALID-DEPS."
  (goto-char (point-min))
  (when (package-lint--lexical-binding-declared-in-header-line-p)
    (let* ((lexbind-line (line-number-at-pos))
           (lexbind-col (1+ (- (match-beginning 1) (line-beginning-position)))))
      (unless (assq 'emacs valid-deps)
        (package-lint--error
         lexbind-line lexbind-col 'warning
         "You should depend on (emacs \"24\") if you need lexical-binding.")))))

(defun package-lint--check-version-regexp-list (valid-deps list rx-start rx-end)
  "Warn about matches of REGEXP when VERSION is not in VALID-DEPS.
LIST is an alist of (VERSION . REGEXP*).
REGEXP is (concat RX-START REGEXP* RX-END) for each REGEXP*."
  (let ((emacs-version-dep (or (cadr (assq 'emacs valid-deps)) '(0))))
    (pcase-dolist (`(,added-in-version . ,regexp) list)
      (when (version-list-< emacs-version-dep added-in-version)
        (goto-char (point-min))
        (while (re-search-forward (concat rx-start regexp rx-end) nil t)
          (unless (let ((ppss (save-match-data (syntax-ppss))))
                    (or (nth 3 ppss) (nth 4 ppss)))
            (package-lint--error
             (line-number-at-pos)
             (save-excursion (goto-char (match-beginning 1)) (current-column))
             'error
             (format "You should depend on (emacs \"%s\") if you need `%s'."
                     (mapconcat #'number-to-string added-in-version ".")
                     (match-string-no-properties 1)))))))))

(defun package-lint--check-libraries-available-in-emacs (valid-deps)
  "Warn about use of libraries that are not available in the Emacs version in VALID-DEPS."
  (package-lint--check-version-regexp-list
   valid-deps
   package-lint--libraries-added-alist
   "(\\s-*?require\\s-*?'\\("
   ;; Match the ending paren so we can be sure it's a single argument
   ;; `require'. If there are additional arguments, we don't want to warn,
   ;; because (require 'foo nil t) indicates an optional dependency and
   ;; (require 'foo "filename") is very uncommon.
   "\\)\\_>\\s-*?)"))

(defun package-lint--check-macros-functions-available-in-emacs (valid-deps)
  "Warn about use of functions/macros that are not available in the Emacs version in VALID-DEPS."
  (package-lint--check-version-regexp-list
   valid-deps
   package-lint--functions-and-macros-added-alist
   "(\\s-*?\\("
   "\\)\\_>"))

(defun package-lint--check-lexical-binding-is-on-first-line ()
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
                (defvar enable-dir-local-variables)
                (defvar hack-local-variables--warned-lexical)
                (let ((hack-local-variables--warned-lexical nil)
                      (enable-dir-local-variables nil)
                      (enable-local-variables t)
                      (local-enable-local-variables t))
                  (hack-local-variables)
                  (setq lexical-binding-found-at-end
                        hack-local-variables--warned-lexical)))
            (error
             (package-lint--error 1 1 'error (error-message-string err))
             (cl-return-from return nil)))
          (when (or lexical-binding-found-at-end
                    ;; In case this is an Emacs from before `hack-local-variables'
                    ;; started to warn about `lexical-binding' on a line other
                    ;; than the first.
                    (and (cdr (assq 'lexical-binding file-local-variables-alist))
                         (not (package-lint--lexical-binding-declared-in-header-line-p))))
            (package-lint--error
             1 1 'error
             "`lexical-binding' must be set in the first line.")))))))

(defun package-lint--check-do-not-depend-on-cl-lib-1.0 (valid-deps)
  "Check that any dependency in VALID-DEPS on \"cl-lib\" is on a remotely-installable version."
  (let ((cl-lib-dep (assq 'cl-lib valid-deps)))
    (when cl-lib-dep
      (let ((cl-lib-version (nth 1 cl-lib-dep)))
        (when (version-list-<= '(1) cl-lib-version)
          (package-lint--error
           (nth 2 cl-lib-dep) (nth 3 cl-lib-dep) 'error
           (format "Depend on the latest 0.x version of cl-lib rather than on version \"%S\".
Alternatively, depend on (emacs \"24.3\") or greater, in which cl-lib is bundled."
                   cl-lib-version)))))))

(defun package-lint--check-package-version-present ()
  "Check that a valid \"Version\" header is present."
  (let ((version (package-lint--goto-header (rx (? "Package-") "Version"))))
    (if version
        (unless (ignore-errors (version-to-list version))
          (package-lint--error
           (line-number-at-pos)
           (1+ (- (match-beginning 3) (line-beginning-position)))
           'warning
           (format "\"%s\" is not a valid version. MELPA will handle this, but other archives will not." version)))
      (package-lint--error
       1 1 'warning
       "\"Version:\" or \"Package-Version:\" header is missing. MELPA will handle this, but other archives will not."))))

(defun package-lint--check-package-el-can-parse ()
  "Check that `package-buffer-info' can read metadata from this file.
If it can, return the read metadata."
  (condition-case err
      (let ((orig-buffer (current-buffer)))
        ;; We've reported version header issues separately, so rule them out here
        (with-temp-buffer
          (insert-buffer-substring-no-properties orig-buffer)
          (package-lint--update-or-insert-version "0")
          (package-buffer-info)))
    (error
     (package-lint--error
      1 1
      'error
      (format "package.el cannot parse this buffer: %s" (error-message-string err)))
     nil)))

(defun package-lint--check-package-summary (desc)
  "Check the summary for package with descriptor DESC.
DESC is a struct as returned by `package-buffer-info'."
  (let ((summary (package-lint--package-desc-summary desc)))
    (cond
     ((string= summary "")
      (package-lint--error
       1 1
       'warning
       "Package should have a non-empty summary."))
     ((> (length summary) 50)
      (package-lint--error
       1 1
       'warning
       "The package summary is too long. It should be at most 50 characters.")))
    (when (save-match-data
            (let ((case-fold-search t))
              (and (string-match "\\<emacs\\>" summary)
                   (not (string-match-p "[[:space:]]+lisp" summary (match-end 0))))))
      (package-lint--error
       1 1
       'warning
       "Including \"Emacs\" in the package description is usually redundant."))))

(defun package-lint--check-symbol-separators (definitions)
  "Check that symbol DEFINITIONS don't contain non-standard separators."
  (pcase-dolist (`(,name . ,position) definitions)
    (when (and (string-match "[:/]" name)
               (not (string-match-p package-lint--sane-prefixes name)))
      (let ((match-pos (match-beginning 0)))
        ;; As a special case, allow `/=' when at the end of a symbol.
        (when (or (not (string-match (rx "/=" string-end) name))
                  (/= match-pos (match-beginning 0)))
          (goto-char position)
          (package-lint--error
           (line-number-at-pos) 1 'error
           (format "`%s' contains a non-standard separator `%s', use hyphens instead."
                   name (substring-no-properties name match-pos (1+ match-pos)))))))))

(defun package-lint--check-defs-prefix (definitions)
  "Verify that symbol DEFINITIONS start with package prefix."
  (let ((prefix (package-lint--get-package-prefix)))
    (when prefix
      (let ((prefix-re (rx-to-string `(seq string-start ,prefix (or "-" string-end)))))
        (pcase-dolist (`(,name . ,position) definitions)
          (unless (or (string-match-p prefix-re name)
                      (string-match-p package-lint--sane-prefixes name))
            (let ((line-no (line-number-at-pos position)))
              (package-lint--error
               line-no 1 'error
               (format "\"%s\" doesn't start with package's prefix \"%s\"."
                       name prefix)))))))))


;;; Helpers

(defun package-lint--lowest-installable-version-of (package)
  "Return the lowest version of PACKAGE available for installation."
  (let ((descriptors (cdr (assq package package-archive-contents))))
    (if (fboundp 'package-desc-version)
        (car (sort (mapcar 'package-desc-version descriptors)
                   #'version-list-<))
      (aref descriptors 0))))

(defun package-lint--goto-header (header-name)
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

(defun package-lint--update-or-insert-version (version)
  "Ensure current buffer has a \"Version: VERSION\" header."
  (if (package-lint--goto-header "Version")
      (move-beginning-of-line nil)
    (forward-line))
  (insert (format ";; Version: %s" version))
  (newline))

(defun package-lint--get-header-line-file-local-variables ()
  "Return local variables specified in the -*- line.
Returns an alist of elements (VAR . VAL), where VAR is a variable
and VAL is the specified value.

For details, see `hack-local-variables-prop-line'."
  (cl-letf (((symbol-function #'message) #'ignore))
    (hack-local-variables-prop-line)))

(defun package-lint--lexical-binding-declared-in-header-line-p ()
  "Test if `lexical-binding' is declared in the -*- line."
  ;; Test the `cdr' to see if it's actually true, because
  ;; -*- lexical-binding: nil -*-
  ;; is legal, if silly.
  (cdr (assq 'lexical-binding (package-lint--get-header-line-file-local-variables))))

(defvar semantic-imenu-summary-function)

(defun package-lint--get-defs ()
  "Return a list of all variables and functions defined in the current buffer.

The returned list is of the form (SYMBOL-NAME . POSITION)."
  ;; We probably could use Semantic instead, but it's a *global* minor mode and
  ;; it tends to be quite heavy, so use Imenu instead; if the user has Semantic
  ;; enabled, Imenu will use its index anyway.
  (let ((result '())
        (index
         (save-excursion
           ;; Use the default imenu expression list so that we're not confused
           ;; by user customizations.
           (let ((imenu-generic-expression lisp-imenu-generic-expression)
                 ;; In case it's actually Semantic, tell it not to decorate
                 ;; symbol names.
                 (semantic-imenu-summary-function 'semantic-format-tag-name))
             (funcall imenu-create-index-function)))))
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
    (nreverse result)))

(defun package-lint--get-package-prefix ()
  "Return package prefix string (i.e. the symbol the package `provide's).
Prefix is returned without any `-mode' suffix."
  (goto-char (point-max))
  (when (re-search-backward (rx "(provide '" (group (1+ (or (syntax word) (syntax symbol))))) nil t)
    (replace-regexp-in-string "-mode\\'" "" (match-string-no-properties 1))))


;;; Public interface

;;;###autoload
(defun package-lint-buffer (&optional buffer)
  "Get linter errors and warnings for BUFFER.

Returns a list, each element of which is list of

   (LINE COL TYPE MESSAGE)

where TYPE is either 'warning or 'error.

Current buffer is used if none is specified."
  (with-current-buffer (or buffer (current-buffer))
    (package-lint--check-all)))

;;;###autoload
(defun package-lint-current-buffer ()
  "Display lint errors and warnings for the current buffer."
  (interactive)
  (let ((errs (package-lint-buffer))
        (buf "*Package-Lint*"))
    (with-current-buffer (get-buffer-create buf)
      (let ((buffer-read-only nil))
        (erase-buffer)
        (cond
         ((null errs) (insert "No issues found."))
         ((null (cdr errs)) (insert "1 issue found:\n\n"))
         (t (insert (format "%d issues found:\n\n" (length errs)))))
        (pcase-dolist (`(,line ,col ,type ,message) errs)
          (insert (format "%d:%d: %s: %s\n" line col type message))))
      (special-mode)
      (view-mode 1))
    (display-buffer buf)))

;;;###autoload
(defun package-lint-batch-and-exit ()
  "Run `package-lint-buffer' on the files remaining on the command line.
Use this only with -batch, it won't work interactively.

When done, exit Emacs with status 0 if there were no errors nor warnings or 1
otherwise."
  (unless noninteractive
    (error "`package-lint-batch-and-exit' is to be used only with -batch"))
  ;; Make sure package.el is initialized so we can query its database.
  (package-initialize)
  (let ((success t))
    (dolist (file command-line-args-left)
      (with-temp-buffer
        (insert-file-contents file t)
        (emacs-lisp-mode)
        (let ((checking-result (package-lint-buffer)))
          (when checking-result
            (setq success nil)
            (message "In `%s':" file)
            (pcase-dolist (`(,line ,col ,type ,message) checking-result)
              (message "  at %d:%d: %s: %s" line col type message))))))
    (kill-emacs (if success 0 1))))

;;;###autoload
(defun package-lint-looks-like-a-package-p ()
  "Return non-nil if the current buffer appears to be intended as a package."
  (save-match-data
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (re-search-forward
         (concat lm-header-prefix
                 (rx (or "Version" "Package-Version" "Package-Requires")))
         nil t)))))

(provide 'package-lint)
;;; package-lint.el ends here
