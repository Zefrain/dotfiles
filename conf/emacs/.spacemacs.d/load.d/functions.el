(defun my-tags-query-replace-compile-replacement (fun &rest args)
  "Apply `q-r-compile-replacement' to `tags-query-replace' in FUN with ARGS."
  (cl-letf* ((old-tags-query-replace (symbol-function 'tags-query-replace))
             ((symbol-function 'tags-query-replace)
              (lambda (from to &rest other-args)
                (apply old-tags-query-replace
                       from
                       (query-replace-compile-replacement to t)
                       other-args))))
    (apply fun args)))

(advice-add 'projectile-replace-regexp :around #'my-tags-query-replace-compile-replacement)
