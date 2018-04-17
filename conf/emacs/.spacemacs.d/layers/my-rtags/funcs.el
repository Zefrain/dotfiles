;; rtags
(defun my-rtags-load-compile-commands-command ()
  "rtags load compile_commands.json command"
  ;; compile_commands.json generate by https://github.com/vincent-picaud/Bazel_and_CompileCommands
  ;; will refer source code from bazel's sandbox, must use "--project-root" to fix it.
  (let ((project-root default-directory)
        (tmp-project-root ""))
    (while (and project-root (not (file-exists-p (concat project-root "compile_commands.json"))))
           (setq tmp-project-root (file-name-directory (directory-file-name project-root)))
           (message "tmp-project-root: %s, project-root: %s" tmp-project-root project-root)
           (if (equal tmp-project-root project-root)
             (setq project-root nil)
             (setq project-root tmp-project-root)))
    (unless project-root
      (message "RTags: compile_commands.json not exists")
      (setq project-root default-directory))
    (message "RTags: %s" (concat project-root "compile_commands.json"))
    (format "rc -J %s --project-root %s" project-root project-root)))

(defun my-rtags-run ()
  "rtags startup with generated compile_commands.json"
  (interactive)
  (rtags-start-process-unless-running)
  (shell-command (my-rtags-load-compile-commands-command)))

(defun my-rtags-build ()
  "rtags startup use compile_commands.json generate from build tool"
  (interactive)
  (cond ((file-exists-p "BUILD") (my-rtags-bazel))
        ((file-exists-p "CMakeLists.txt") (my-rtags-cmake))
        ((file-exists-p "Makefile") (my-rtags-make))
        (t (error "No build tool detected"))))

(defun my-rtags-bazel ()
  "rtags startup use compile_commands.json generate from bazel"
  (interactive)
  (let ((tool_dir "~/Opensource/Bazel_and_CompileCommands")
        (command ""))
    (setq command (format "%s/setup_compile_commands.sh; %s/create_compile_commands.sh //..." tool_dir tool_dir))
    (setq command (read-string "Build bazel compile_commands.json: " command nil nil))
    (unless command
      (error "Build compile_commands.json for bazel failed"))
    (rtags-start-process-unless-running)
    (async-shell-command (concat command " && " (my-rtags-load-compile-commands-command)))))

(defun my-rtags-make ()
  "build compile_commands.json for make"
  (interactive)
  (let ((command (read-string "Build make compile_commands.json: " "bear make" nil nil)))
    (unless command
      (error "Build compile_commands.json for make failed"))
    (rtags-start-process-unless-running)
    (async-shell-command (concat command " && " (my-rtags-load-compile-commands-command)))))

(defun my-rtags-cmake ()
  "build compile_commands.json for cmake"
  (interactive)
  (let ((command (read-string "Build cmake compile_commands.json: " "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ." nil nil)))
    (rtags-start-process-unless-running)
    (async-shell-command (concat command " && " (my-rtags-load-compile-commands-command)))))
