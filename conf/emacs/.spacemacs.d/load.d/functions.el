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


(defun occur-mode-clean-buffer ()
  "Removes all commentary from the *Occur* buffer, leaving the
 unadorned lines."
  (interactive)
  (if (get-buffer "*Occur*")
      (save-excursion
        (set-buffer (get-buffer "*Occur*"))
        (goto-char (point-min))
        (toggle-read-only 0)
        (if (looking-at "^[0-9]+ lines matching \"")
            (kill-line 1))
        (while (re-search-forward "^[ \t]*[0-9]+:"
                                  (point-max)
                                  t)
          (replace-match "")
          (forward-line 1)))
    (message "There is no buffer named \"*Occur*\".")))


(defun func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))


(defun hex-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))


(defun unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))


;; org-todo
(defun org-insert-todo-return (&optional ARG INVISIBLE-OK TOP)
  "Do org-todo before and then insert org-heading"
  (interactive "P")
  (org-todo ARG)
  (org-insert-heading ARG INVISIBLE-OK TOP)
  (org-todo ARG)
  (end-of-line))


;; load current-buffer
(defun load-buffer-file()
  "load current buffer"
  (interactive)
  (if (eq major-mode 'emacs-lisp-mode)
      (load-file (buffer-file-name))
    (message "This isn't a emacs lisp file")))


;; goto project changelog
(defun org-projectile/goto-changelog()
  "goto projectile changelog"
  (interactive)
  (org-projectile-goto-location-for-project (projectile-project-name)))
