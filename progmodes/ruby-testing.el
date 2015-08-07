;;; ================ running RSpec tests ================

(defun my-rails--seed-arg-string (seed)
  "Returns \"--seed=SEED\". If SEED is 1, returns \"--seed=$RANDOM\"."
  (concat "--seed="
          (if (equal seed 1) "$RANDOM" (int-to-string seed))))

(defun my-rails--rspec-command (seed fname)
  (let* ((rails-root (find-rails-root (file-name-directory fname)))
         (rspec-cmd (if (file-exists-p (concat rails-root "bin/rspec"))
                        "bin/rspec"
                      "rspec")))
    (concat "cd " rails-root " && "
            "echo > log/test.log && "
            "RAILS_ENV=test " rspec-cmd " "
            (my-rails--seed-arg-string seed) " " fname)))

(defun my-rails--rspec-at-point-command (seed fname line-number)
  (concat (my-rails--rspec-command seed fname)
          ":" (int-to-string line-number)))


(defun run-spec-using (run-func seed fname)
  "Run RSpec test FNAME from the rails root directory above it.
If SEED is 1, $RANDOM will be used. Calls
`my-rails--rspec-command' to generated the command to run.
RUN-FUNC must be a function such as `compile' that takes a string
and executes it."
  (funcall run-func (my-rails--rspec-command seed fname)))

(defun run-spec (seed fname)
  "Run RSpec test FNAME from the Rails root directory above it.
If SEED is 1, $RANDOM will be used. FNAME may contain extra line
number info (e.g., 'foo.rb::42')."
  (interactive "p\nF") ; possibly nonexistent file name so we can append ":NNN"
  (run-spec-using #'compile seed fname))

(defun run-spec-in-iterm (seed fname)
  "Run RSpec test at point in iTerm. If SEED is 1, $RANDOM will
be used."
  (interactive "p\nF") ; possibly nonexistent file name so we can append ":NNN"
  (run-spec-using #'send-to-iterm seed fname))


(defun run-spec-at-point-using (func seed)
  "Run RSpec test FNAME from the rails root directory above it.
If SEED is 1, $RANDOM will be used. Calls
`my-rails--rspec-at-point-command' to generated the command to run.
RUN-FUNC must be a function such as `compile' that takes a string
and executes it."
  (funcall func (my-rails--rspec-at-point-command
                 seed
                 (buffer-file-name)
                 (line-number-at-pos))))

(defun run-spec-at-point (seed)
  "Run RSpec test at point from the Rails root directory. If SEED is 1,
$RANDOM will be used."
  (interactive "p")
  (run-spec-at-point-using #'compile seed))

(defun run-spec-at-point-in-iterm (seed)
  "Run RSpec test at point in iTerm. If SEED is 1, $RANDOM will
be used."
  (interactive "p")
  (run-spec-at-point-using #'send-to-iterm seed))

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
