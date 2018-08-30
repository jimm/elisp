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

;;; ================ older test runners ================

(provide 'ruby-testing)
