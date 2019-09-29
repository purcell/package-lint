;;; sym-diff.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'seq)

(defun sym-diff-hashdiff (v1 v2)
  (let ((in-v2 (make-hash-table)))
    (dolist (x v2)
      (puthash x t in-v2))
    (seq-filter (lambda (e) (not (gethash e in-v2))) v1)))

(defun sym-diff (dump1 dump2 type)
  (let ((v1 (cdr (assoc type dump1)))
        (v2 (cdr (assoc type dump2))))
    (sort (sym-diff-hashdiff v1 v2)
          (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun sym-diff-load-dump (f)
  (with-temp-buffer
    (insert-file-contents f)
    (goto-char 0)
    (read (current-buffer))))

(defun sym-diff-versions (v1 v2)
  `((functions . ((removed . ,(sym-diff v1 v2 'functions))
                  (added . ,(sym-diff v2 v1 'functions))))
    (variables . ((removed . ,(sym-diff v1 v2 'variables))
                  (added . ,(sym-diff v2 v1 'variables))))
    (features . ((removed . ,(sym-diff v1 v2 'features))
                 (added . ,(sym-diff v2 v1 'features))))
    ))

(defun sym-diff-dumps (f1 f2)
  (let ((v1 (sym-diff-load-dump f1))
        (v2 (sym-diff-load-dump f2)))
    (sym-diff-versions v1 v2)))

;;; sym-diff.el ends here
