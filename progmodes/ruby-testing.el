;;; ================ running RSpec tests ================

(defun my-rails--seed-arg-string (seed)
  "Returns \"--seed=SEED\". If SEED is 1, returns \"--seed=$RANDOM\"."
  (concat "--seed="
          (if (equal seed 1) "$RANDOM" (int-to-string seed))))

(defun my-rails--rspec-command (seed fname)
(interactve)                            ;DEBUG
  (let* ((rails-root (find-rails-root (file-name-directory fname)))
         (rspec-cmd (if (file-exists-p (concat rails-root "bin/rspec"))
                        "bin/rspec"
                      "rspec")))
    (concat "cd " rails-root " && "
            "echo > log/test.log && "
            "RAILS_ENV=test bundle exec " rspec-cmd " "
            (my-rails--seed-arg-string seed) " " fname)))

(defun my-rails--rspec-at-point-command (seed fname)
  (concat (my-rails--rspec-command seed fname)
          ":" (int-to-string (line-number-at-pos))))

(defun run-spec (seed fname)
  "Run RSpec test FNAME from the $my-rails directory. If SEED is 1, $RANDOM
will be used. FNAME may contain extra line number info (e.g., 'foo.rb::42')."
  (interactive "p\nF") ; possibly nonexistent file name so we can append ":NNN"
  (compile (my-rails--rspec-command seed fname)))

(defun run-spec-at-point (seed)
  "Run RSpec test at point from the $my-rails directory. If SEED is 1,
$RANDOM will be used."
  (interactive "p")
  (compile (my-rails--rspec-at-point-command seed (buffer-file-name))))

(defun run-spec-at-point-in-iterm (seed)
  "Run RSpec test at point in iTerm. If SEED is 1, $RANDOM will be used."
  (interactive "p")
  (tell-iterm (my-rails--rspec-at-point-command seed (buffer-file-name))))

;;; ================ older test runners ================


(defun run-ruby-test (test-name)
  "Will run TEST-NAME from the current buffer's file, which is
presumed to be a test file. If TEST-NAME is empty or nil, runs
all tests in the file.

If *ruby-test-inject-command* is defined it is run after changing
to the root dir and before running the test. For example, you can
use this to delete the log/test.log file."
  (interactive "sTest name (empty for all tests in the file): ")
  (let ((root-dir (locate-dominating-file (file-name-directory (buffer-file-name)) "Rakefile")))
    (if root-dir
        (let ((root-relative-file (substring (buffer-file-name) (length (expand-file-name root-dir)))))
          (save-buffer)
          (compile (concat "cd " (shell-quote-argument (expand-file-name root-dir))
                           (when (boundp '*ruby-test-inject-command*)
                             (concat " && " *ruby-test-inject-command*))
                           " && ruby -I test " root-relative-file
                           (when (> (length test-name) 0)
                             (concat " -n " test-name)))))
      (error "Can not find RAILS_ROOT (Rakefile not found)"))))

(defun run-ruby-spec (spec-name-fragment)
  "Will run all specs whose full names include SPEC-NAME-FRAGMENT
from the current buffer's file, which is presumed to be an RSpec
test file. If SPEC-NAME-FRAGMENT is empty or nil, runs all tests
in the file.

If *ruby-test-inject-command* is defined it is run after changing
to the root dir and before running the test. For example, you can
use this to delete the log/test.log file."
  (interactive "sSpec name fragment (empty for all tests in the file): ")
  (let ((root-dir (locate-dominating-file (file-name-directory (buffer-file-name)) "Rakefile")))
    (if root-dir
        (let ((root-relative-file (substring (buffer-file-name) (length (expand-file-name root-dir)))))
          (save-buffer)
          (compile (concat "cd " (shell-quote-argument (expand-file-name root-dir))
                           (when (boundp '*ruby-test-inject-command*)
                             (concat " && " *ruby-test-inject-command*))
                           " && rspec " root-relative-file
                           (when (> (length spec-name-fragment) 0)
                             (concat " -e " (shell-quote-argument spec-name-fragment))))))
      (error "Can not find RAILS_ROOT (Rakefile not found)"))))


(provide 'ruby-testing)
