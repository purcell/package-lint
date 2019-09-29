;;; sym-dump.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:

;; This is all written in a weird way in order to minimise the code loaded
;; during its execution.

;;; Code:

(defun sym-dump-filter-atoms (pred)
  (let (result)
    (mapatoms
     (lambda (f)
       (when (and (funcall pred f)
                  (not (string-prefix-p "sym-dump-" (symbol-name f))))
         (push f result))))
    (sort result (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun sym-dump-defined-functions ()
  (sym-dump-filter-atoms 'fboundp))

(defun sym-dump-defined-vars ()
  (sym-dump-filter-atoms 'boundp))

(defun sym-dump-defined-faces ()
  (sym-dump-filter-atoms 'facep))

(defun sym-dump-libraries (path)
  (let (libs)
    (dolist (dir path)
      (when (file-accessible-directory-p dir)
        (dolist (f (directory-files dir t "\\.elc?\\'" t))
          (let ((lib (intern (file-name-base f))))
            ;; Skip files that aren't loadable libraries, e.g. blessmail, edt-mapper, dunnet
            (when (with-temp-buffer
                    (insert-file-contents f)
                    (goto-char 0)
                    (re-search-forward (format "(provide '%s)" lib) nil t))
              (push lib libs))))))
    libs))

(defun sym-dump-loaded ()
  `((variables . ,(sym-dump-defined-vars))
    (functions . ,(sym-dump-defined-functions))
    (features . ,features)
    (faces . ,(sym-dump-defined-faces))))

(defun sym-dump-go-crazy ()
  (dolist (lib (sym-dump-libraries load-path))
    (message "Loading %s" lib)
    (with-demoted-errors (require lib nil t)))
  (message "Loaded all")
  (pp (sym-dump-loaded)))

(setq debug-on-signal t
      debug-on-quit t)
(sym-dump-go-crazy)

;;; sym-dump.el ends here
