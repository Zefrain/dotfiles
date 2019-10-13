;; platnuml
(with-eval-after-load 'plantuml-mode
  (setq plantuml-jar-path     (concat (shell-command-to-string "echo -ne $(brew --prefix plantuml)") "/libexec/plantuml.jar"))
  (setq org-plantuml-jar-path (concat (shell-command-to-string "echo -ne $(brew --prefix plantuml)") "/libexec/plantuml.jar")))

;; winum
(setq winum-scope (quote frame-local))


;; coding system
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; backup-file
(setq backup-directory-alist (quote ((".*" . "~/.spacemacs.d/backup"))))
;; (setq make-backup-file-name-function (quote backup-file-name-function-custom))

(configuration-layer/remove-layer 'web-mode)

(setq comment-style 'indent)

;; set warning level
(ignore 'defvaralias)

(setq warning-minimum-level ':error)
(setq dotspacemacs-mode-line-theme '(all-the-icons :separator 'arrow))


;; line break
(setq truncate-lines t)

;; vc-control
(setq vc-follow-symlinks nil)

;; org
(setq org-export-with-sub-superscripts nil)
(setq org-export-with-sub-superscripts (quote {}))

;; org-projectile
(with-eval-after-load 'org-projectile
  (setq org-projectile-file "doc/TODOs.org"))

;; org-agenda
(defun push-agenda (file)
  "push file to org-agenda-files"
  (when (file-exists-p file)
    (push file org-agenda-files)))

(with-eval-after-load 'org-agenda
  (require 'org-projectile)
  (mapcar 'push-agenda (org-projectile-todo-files))
  (require 'org-journal)
  (push-agenda org-journal-dir))

(setq vc-follow-symlinks t)
