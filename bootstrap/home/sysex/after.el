(lighten-up)

(defvar work-orgs-dir "candi"
  "Name of $pim/orgs/work subdir where I keep work-related Org mode files.")

(setq dired-use-ls-dired nil)

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(defvar compile-ant "ant -e -s build.xml ")
(defvar compile-rake "rake test ")

(set-register ?n compile-ant)
(set-register ?q "rake test ")

(setq sql-sqlite-program "sqlite3")

(when (fboundp #'fzf)
  (setq fzf/executable "~/.fzf/bin/fzf"))

;; Fix for Max OS X 10.11.1 El Capitan problem
(setq visible-bell nil)
(setq ring-bell-function #'mode-line-visible-bell)

;; Start Emacs server
(server-start)
