;;; For my consulting at Particle Health.

(setq org-agenda-files (list
                        (concat *my-pim-dir* "orgs/work/seat_geek/todo.org")
                        (concat *my-pim-dir* "orgs/todo.org"))
      gcp-project-ids '(("d-api" . "particle-health-scratch")
                        ("d-data" . "prj-d-data")
                        ("d-sandbox" . "mythical-style-302819")
                        ("p-api" . "particle-health-prod")
                        ("p-data" . "prj-p-data")
                        ("p-sandbox" . "dev-fortress-302415")
                        ("s-api" . "particle-health-staging")
                        ("s-data" . "prj-s-data")
                        ("s-sandbox" . "geometric-ivy-289820"))
      *status-file* (concat *my-pim-dir*
                            "orgs/work/particle_health/status_"
                            (format-time-string "%Y")
                            ".org"))

(defvar work-orgs-dir "particle_health"
  "Name of $pim/orgs/work subdir where I keep work-related Org mode files.")

(defvar smoke-tests-command
  (concat (getenv "box") "/bin/smoke")
  "Command used to run the smoke tests")

(defun gcp-project-id (name)
  "Returns a project name given a short environment `name' like \"d-api\"."
  (interactive "sProject short name (d-api): ")
  (let* ((sname (if (string-empty-p name) "d-api" name))
         (project-id (cdr (assoc sname gcp-project-ids))))
    (message project-id)
    project-id))

(defun smoke-tests--val (s)
  "Returns a smoke test skip string. Given a known abbreviation, returns
the full name. Otherwise returns `s`."
  (cond ((or (equal s "d") (equal s "default")) "monolith-default")
        ((or (equal s "a") (equal s "alternate")) "monolith-alternate")
        ((or (equal s "n") (equal s "nucleus")) "monolith-nucleus")
        ((or (equal s "v") (equal s "v2queryflow")) "monolith-v2queryflow")
        ((equal s "m") "management")
        s))

(defun smoke-tests (&optional skip-tests)
  "Runs the quark smoke tests, optionally skipping specified test groups."
  (interactive "sTests to run ([m]anagement,[d]efault,[a]lternate,[n]ucleus,[v]2queryflow,[A]ll): ")
  (let ((default-directory (getenv "quark"))
        (tests-to-skip (list "management" "monolith-default" "monolith-alternate"
                             "monolith-nucleus" "monolith-v2queryflow")))
    (if (equal skip-tests "")
        (setq tests-to-skip '())
      (mapc (lambda (ch)
              (cond ((equal ch ?A) (setq tests-to-skip '()))
                    ((equal ch ?m) (setq tests-to-skip (remove "management" tests-to-skip)))
                    ((equal ch ?d) (setq tests-to-skip (remove "monolith-default" tests-to-skip)))
                    ((equal ch ?a) (setq tests-to-skip (remove "monolith-alternate" tests-to-skip)))
                    ((equal ch ?n) (setq tests-to-skip (remove "monolith-nucleus" tests-to-skip)))
                    ((equal ch ?v) (setq tests-to-skip (remove "monolith-v2queryflow" tests-to-skip)))))
            (string-to-list skip-tests)))
    (compile (concat "cd $quark && TESTS_TO_SKIP=" (string-join tests-to-skip ",") " " smoke-tests-command))))

(add-to-list 'auto-mode-alist '("\\.proto$" . javascript-mode)) ; good enough for now

;; Org mode ph-sc links

(defun my-org-mode-ph-shortcut-link (tag)
  "Given a TAG of the form '<number>', returns a URL to a Shortcut ticket."
  (concat "https://app.shortcut.com/particlehealth/story/" tag))

(add-to-list 'org-link-abbrev-alist
             '("ph-sc" . "%(my-org-mode-ph-shortcut-link)"))

(put 'my-org-mode-ph-shortcut-link 'org-link-abbrev-safe t)

;; ---------------- testing ----------------

(defun ph-go-test (arg)
  "Run go tests in the directory containing the current buffer's file from
the project root dir.

A numeric argument of 4 causes the test in which the cursor resides to run."
  (interactive "p")
  (let* ((dir (locate-dominating-file default-directory #'makeup-dir-p))
         (dir-path (abbreviate-file-name default-directory))
         (relative-dir-path (substring dir-path (length dir)))
         (single-test-to-run (when (= arg 4)
                               (save-excursion
                                 (search-backward "\nfunc Test")
                                 (forward-word 2)
                                 (word-at-point)))))
    ;; We tee to /tmp/compile.out because Emacs truncates long error lines
    ;; and in a compile buffer expanding them can be a pain.
    (compile (concat "cd " dir " && go test ./" relative-dir-path
                     (when single-test-to-run
                       (concat " -run " single-test-to-run))
                     " | tee /tmp/compile.out"))))

;; ---------------- Claude ----------------
;; https://github.com/stevemolitor/claude-code.el

;; for eat terminal backend (the default):
(use-package eat :ensure t)

;; ;; for vterm terminal backend:
;; (use-package vterm :ensure t)

;; install claude-code.el
(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  (claude-code-mode)
  (setq claude-code-newline-keybinding-style 'super-return-to-send)
  :bind-keymap ("C-c c" . claude-code-command-map))

;; ---------------- Spanner ----------------

(load (concat *my-emacs-lib-dir* "sql-spanner"))

(setq sql-user "particle-health-scratch"
      sql-server "particle"
      sql-database "particle"
      *my-sql-regex* "\\([^;]\\)\\'"
      *my-sql-regex-replacement* "\\&\\\\G"
      sql-send-terminator t)            ; use the spanner :terminator cons


(defun spanner (env-name)
  "Runs sql-spanner after translating `env-name' to a GCP project id and
using that as the default username. The username is interpreted by
sql-spanner as the GCP project id."
  ("sProject short name (d-api): ")
  (set sql-user (gcp-project-id (if (string-empty-p env-name) "d-api" env-name)))
  (sql-spanner))
