;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers")
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(javascript
     go
     pandoc
     auto-completion
     syntax-checking
     cnfonts
     (gtags :variables
            gtags-enable-by-default t)
     (python :variables
             python-backend 'anaconda
             python-enable-yapf-format-on-save t)
     lsp
     (c-c++ :variables
            c-c++-enable-rtags-support t
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t)
     ;; colors
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     better-defaults
     (git :variables
          magit-repository-directories '("~/Private/"
                                         "~/Public/"
                                         "~/Documents/"))
     (org :variables
          org-bullets-bullet-list '("♥" "■" "◆" "▲" "▶")
          org-enable-org-journal-support t
          org-journal-dir "~/Private/Notes/journal/"
          org-journal-file-format "%Y-%m-%d.org"
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%A, %B %d %Y"
          org-journal-time-prefix "* "
          org-journal-time-format ""
          org-projectile-file "org/TODOs.org"
          org-enable-github-support t
          org-capture-templates
          (quote
           (("t" "Todo" entry
             (file "~/Private/org/TODOs.org")
             "* TODO %? %t \n %? \n" :empty-lines 1 :jump-to-captured t)
            ("z" "zxpay/CHANGELOG" entry
             (file+headline "~/Private/Working/zxpay/org/CHANGELOG.org" "Release 0.x.x")
             "* %? %t\n** New\n** Fix" :prepend t :jump-to-captured t))))

     (markdown :variables markdown-live-preview-engine 'vmd)
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     (shell :variables
            shell-default-shell 'eshell
            shell-default-height 20
            shell-default-position 'bottom
            shell-enable-smart-eshell t)
     (spacemacs-layouts :variables
                        layouts-enable-autosave nil
                        layouts-autosave-delay 300)
     (osx :variables
          osx-command-as       'hyper
          osx-option-as        'meta
          osx-control-as       'control
          osx-function-as      'none
          osx-right-command-as 'left
          osx-right-option-as  'left
          osx-right-control-as 'left))
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      cquery
                                      grab-mac-link)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; lastest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 15
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil

   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t

   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil

   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil

   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil

   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom

   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always

   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%b"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  ;; (setq configuration-layer--elpa-archives
  ;;       '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
  ;;         ("org-cn"   . "http://elpa.emacs-china.org/org/")
  ;;         ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")
  ;;         ))

  ;; (setq configuration-layer-elpa-archives
  ;;       '(
  ;;         ("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
  ;;         ("org-cn"   . "http://elpa.emacs-china.org/org/")
  ;;         ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")
  ;;         ))

  )

(defun my-xref/find-definitions ()
  (interactive)
  (if lsp-mode (lsp-ui-peek-find-definitions) (spacemacs/jump-to-definition)))

(defun my-xref/find-references ()
  (interactive)
  (if lsp-mode (lsp-ui-peek-find-references) (spacemacs/jump-to-reference)))

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
(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; lsp
  (add-hook 'c-mode-hook #'lsp-cquery-enable)
  (add-hook 'c++-mode-hook #'lsp-cquery-enable)
  (add-hook 'python-mode-hook #'lsp-python-enable)

  (use-package company-lsp
    :defer t
    :init
    (setq company-quickhelp-delay 0)
    ;; Language servers have better idea filtering and sorting,
    ;; don't filter results on the client side.
    (setq company-transformers nil
          company-lsp-async t
          company-lsp-cache-candidates nil)
    (spacemacs|add-company-backends :backends company-lsp :modes c-mode c++-mode python-mode go-mode)
    ) ;; lsp


  ;; cquery
  (setq cquery-executable "/usr/local/bin/cquery")
  (define-key evil-motion-state-map (kbd "C-c c ,") #'my-xref/find-references)
  (define-key evil-motion-state-map (kbd "C-c c .") #'my-xref/find-definitions)
  ;; Make C-c C-c behave like C-u C-c C-c in Python mode

  (global-company-mode)
  (require 'python)
  (define-key python-mode-map (kbd "C-c C-c")
    (lambda () (interactive) (python-shell-send-buffer t)))

  ;; wb input method
  (register-input-method
   "chinese-wbim" "euc-cn" 'chinese-wbim-use-package
   "五笔" "汉字五笔输入法" "wb.txt")

  ;; orgmode pdf
  (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                                "xelatex -interaction nonstopmode %f"))

  ;; google-c-style
  (add-hook 'c-mode-common-hook 'google-set-c-style)

  ;; cnfonts
  (require 'cnfonts)
  (setq cnfonts-directory "~/.spacemacs.d/cnfonts/")
  (setq cnfonts-profiles '("Darwin" "Linux"))
  ;; 让 cnfonts 随着 Emacs 自动生效。
  (cnfonts-enable)
  ;; 让 spacemacs mode-line 中的 Unicode 图标正确显示。
  (cnfonts-set-spacemacs-fallback-fonts)

  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)

  (prefer-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)

  ;; (prefer-coding-system 'gb2312-dos)
  ;; (setq default-buffer-file-coding-system 'gb2312-dos)

  ;; grab-mac-link
  (add-hook 'org-mode-hook (lambda ()
                             (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

  ;; git
  (global-git-commit-mode t)

  ;; yasnippet
  (yas-global-mode)

  ;; (add-hook 'c-mode-hook 'flycheck-mode)
  ;; (add-hook 'c++-mode-hook 'flycheck-mode)
  ;; (when (not (display-graphic-p))
  ;;   (setq flycheck-indication-mode nil))

  ;; auto-comletion
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

  ;; backup-file
  (setq backup-directory-alist (quote ((".*" . "~/.spacemacs.d/backup"))))
  (setq make-backup-file-name-function (quote backup-file-name-function-custom))
  (setq org-export-with-sub-superscripts (quote {}))

  ;; winum
  (setq winum-scope (quote frame-local))

  ;; rtags
  (use-package company-rtags
    :defer t
    :init
    (setq rtags-completions-enabled t)
    (eval-after-load 'company
      '(add-to-list
        'company-backends 'company-rtags))
    (setq rtags-autostart-diagnostics t)
    (rtags-enable-standard-keybindings)
    (spacemacs|add-company-backends :backends company-lsp :modes c-mode c++-mode)
    )
  ;; (define-key c-mode-map (kbd "C-c C-j") 'rtags-find-symbol)
  ;; (define-key c++-mode-map (kbd "C-c C-j") 'rtags-find-symbol)
  ;; (define-key c-mode-map (kbd "C-c C-b") 'rtags-location-stack-back)
  ;; (define-key c++-mode-map (kbd "C-c C-b") 'rtags-location-stack-back)
  ;; (define-key c-mode-map (kbd "C-c C-r") 'rtags-find-references)
  ;; (define-key c++-mode-map (kbd "C-c C-r") 'rtags-find-references)
  )


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
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(exec-path-from-shell-check-startup-files nil)
 '(package-selected-packages
   (quote
    (cquery yasnippet-snippets yapfify xterm-color ws-butler winum which-key web-beautify volatile-highlights vmd-mode vi-tilde-fringe uuidgen use-package unfill toc-org symon string-inflection spaceline-all-the-icons smeargle shell-pop reveal-in-osx-finder restart-emacs realgud rainbow-delimiters pyvenv pytest pyenv-mode py-isort popwin pippel pipenv pip-requirements persp-mode pcre2el pbcopy password-generator paradox pandoc-mode ox-pandoc ox-gfm osx-trash osx-dictionary orgit org-projectile org-present org-pomodoro org-mime org-journal org-download org-bullets org-brain open-junk-file neotree mwim multi-term move-text mmm-mode markdown-toc magit-gitflow lsp-ui lsp-python lorem-ipsum livid-mode live-py-mode linum-relative link-hint launchctl json-mode js2-refactor js-doc indent-guide importmagic ibuffer-projectile hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-rtags helm-pydoc helm-purpose helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag grab-mac-link google-translate google-c-style golden-ratio godoctor go-tag go-rename go-guru go-eldoc gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md ggtags fuzzy font-lock+ flycheck-rtags flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help editorconfig dumb-jump disaster diminish cython-mode counsel-projectile company-tern company-statistics company-rtags company-lsp company-go company-c-headers company-anaconda column-enforce-mode coffee-mode cnfonts clean-aindent-mode clang-format centered-cursor-mode auto-yasnippet auto-highlight-symbol aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
)
