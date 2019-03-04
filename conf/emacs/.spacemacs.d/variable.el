(setq plantuml-jar-path (concat (shell-command-to-string "echo -ne $(brew --prefix plantuml)") "/libexec/plantuml.jar"))
(setq org-plantuml-jar-path (concat (shell-command-to-string "echo -ne $(brew --prefix plantuml)") "/libexec/plantuml.jar"))
