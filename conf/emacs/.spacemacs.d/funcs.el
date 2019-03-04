(defun format-declaration (&optional style)
  (progn
    (clang-format-region (region-beginning) (region-end) style)
    (align (region-beginning) (region-end))
    (message "Formatted and aligned region")))

(defun backup-file-name-function-custom(file)
  "Custom function for `make-backup-file-name'.
 Normally this just returns FILE's name with `.%Y%m%d%H%M%S' appended.
 It searches for a match for FILE in `backup-directory-alist'.
 If the directory for the backup doesn't exist, it is created."
  (if (and (eq system-type 'ms-dos)
           (not (msdos-long-file-names)))
      (let ((fn (file-name-nondirectory file)))
        (concat (file-name-directory file)
                (or (and (string-match "\\`[^.]+\\'" fn)
                         (concat (match-string 0 fn) (format-time-string ".%Y%m%d%H%M%S")))
                    (and (string-match "\\`[^.]+\\.\\(..?\\)?" fn)
                         (concat (match-string 0 fn) (format-time-string ".%Y%m%d%H%M%S"))))))
    (concat (make-backup-file-name-1 file) (format-time-string ".%Y%m%d%H%M%S"))))

(defun format-declaration (&optional style)
  "Format the current region or buffer with clang-format according to STYLE."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (clang-format-region (region-beginning) (region-end) style)
          (align (region-beginning) (region-end) )
          (message "Formatted region")))))
