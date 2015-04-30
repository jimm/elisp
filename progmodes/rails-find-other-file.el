;; default-directory is a variable that holds name of directory of current
;; buffer.

(defun rails-root-dir-p (dir)
  "Returns t if dir is a Rails root directory."
  (interactive "D")
  (and (file-exists-p dir)
       (file-directory-p dir)
       (file-exists-p (concat dir "Rakefile"))
       (file-exists-p (concat dir "app"))
       (file-exists-p (concat dir "config"))))

(defun file-system-root-dir-p (dir)
  "Returns t if dir is the root directory of the file system."
  (interactive "D")
  (and (file-exists-p dir)
       (file-directory-p dir)
       (equal dir "/")))

(defun rails-root-dir (path)
  "Finds the Rails root directory that is at or above path."
  (interactive "G")
  (cond ((rails-root-dir-p path) path)
	((file-system-root-dir-p path) nil)
	((file-directory-p path)
	 (rails-root-dir (file-name-directory (directory-file-name path))))
	(t (rails-root-dir (file-name-directory path)))))

;; (defmacro rails-make-rails-app-dir-func (which)
;;   '(defun ,(intern (concat "rails-" which "-dir")) (path)
;;      (concat (rails-root-dir path) "app/" ,which "/")))

(defun rails-config-dir (path)
  (concat (rails-root-dir path) "app/config/"))
(defun rails-lib-dir (path)
  (concat (rails-root-dir path) "app/lib/"))
(defun rails-models-dir (path)
  (concat (rails-root-dir path) "app/models/"))
(defun rails-views-dir (path)
  (concat (rails-root-dir path) "app/views/"))
(defun rails-helpers-dir (path)
  (concat (rails-root-dir path) "app/helpers/"))
(defun rails-javascripts-dir (path)
  (concat (rails-root-dir path) "app/assets/javascripts/"))
(defun rails-stylesheets-dir (path)
  (concat (rails-root-dir path) "app/assets/stylesheets/"))
(defun rails-test-dir (path)
  (concat (rails-root-dir path) "test/"))
(defun rails-spec-dir (path)
  (concat (rails-root-dir path) "spec/"))
(defun rails-features-dir (path)
  (concat (rails-test-dir path) "features/"))
(defun rails-test-fixtures-dir (path)
  (concat (rails-test-dir path) "fixtures/"))
(defun rails-test-functional-dir (path)
  (concat (rails-test-dir path) "functional/"))
(defun rails-test-integration-dir (path)
  (concat (rails-test-dir path) "integration/"))
(defun rails-test-mocks-dir (path)
  (concat (rails-test-dir path) "mocks/"))
(defun rails-test-unit-dir (path)
  (concat (rails-test-dir path) "unit/"))

(defun rails-find-config ()
  (interactive)
  (ido-find-file-in-dir (rails-config-dir default-directory)))
(defun rails-find-lib ()
  (interactive)
  (ido-find-file-in-dir (rails-lib-dir default-directory)))
(defun rails-find-controller ()
  (interactive)
  (ido-find-file-in-dir (rails-controllers-dir default-directory)))
(defun rails-find-model ()
  (interactive)
  (ido-find-file-in-dir (rails-models-dir default-directory)))
(defun rails-find-view ()
  (interactive)
  (ido-find-file-in-dir (rails-views-dir default-directory)))
(defun rails-find-helper ()
  (interactive)
  (ido-find-file-in-dir (rails-helpers-dir default-directory)))
(defun rails-find-javascript ()
  (interactive)
  (ido-find-file-in-dir (rails-javascripts-dir default-directory)))
(defun rails-find-stylesheet ()
  (interactive)
  (ido-find-file-in-dir (rails-stylesheets-dir default-directory)))
(defun rails-find-test ()
  (interactive)
  (ido-find-file-in-dir (rails-test-dir default-directory)))
(defun rails-find-spec ()
  (interactive)
  (ido-find-file-in-dir (rails-spec-dir default-directory)))
(defun rails-find-feature ()
  (interactive)
  (ido-find-file-in-dir (rails-features-dir default-directory)))
(defun rails-find-fixture ()
  (interactive)
  (ido-find-file-in-dir (rails-fixtures-dir default-directory)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-prefix-command 'rails-find-file-map)
            (local-set-key "\C-cg" 'rails-find-file-map)
            (define-key rails-find-file-map "c" 'rails-find-controller)
            (define-key rails-find-file-map "f" 'rails-find-feature)
            (define-key rails-find-file-map "g" 'rails-find-config)
            (define-key rails-find-file-map "h" 'rails-find-helper)
            (define-key rails-find-file-map "j" 'rails-find-javascript)
            (define-key rails-find-file-map "l" 'rails-find-lib)
            (define-key rails-find-file-map "m" 'rails-find-model)
            (define-key rails-find-file-map "s" 'rails-find-spec)
            (define-key rails-find-file-map "t" 'rails-find-test)
            (define-key rails-find-file-map "v" 'rails-find-view)
            (define-key rails-find-file-map "x" 'rails-find-fixture)
            (define-key rails-find-file-map "y" 'rails-find-stylesheet)))

(defun rails-find-other-file ()
  "Finds the \"other\" file, \"other\" being defined depending upon the
current buffer."
  (interactive)
  (let ((path (buffer-file-name)))
    (find-file
     (cond ((or (rails-unit-test-file-p path) (rails-spec-file-p path)) (rails-model-file path))
	   ((rails-fixture-p path) (rails-unit-test-file path))
	   ((rails-model-file-p path) (rails-unit-test-file path))
	   ((rails-api-test-file-p path) (rails-controller-file path))
	   ((rails-functional-test-file-p path) (rails-controller-file path))
	   ((rails-controller-file-p path) (rails-functional-test-file path))
	   ((rails-api-file-p path) (rails-api-test-file path))
	   ((rails-view-file-p path) (rails-controller-from-view-file path))))))

;; ================================================================
;; unused
;; ================================================================

(defun rails-unit-test-file-p (path) (string-match "/test/unit/" path))
(defun rails-spec-file-p (path) (string-match "/spec/" path))

(defun rails-fixture-p (path) (string-match "/test/fixtures/" path))

(defun rails-api-test-file-p (path)
  (string-match "/test/functional/.*_api_" path))

(defun rails-functional-test-file-p (path)
  (string-match "/test/functional/" path))

(defun rails-model-file-p (path) (string-match "/app/models/" path))
(defun rails-view-file-p (path) (string-match "/app/views/" path))
(defun rails-controller-file-p (path) (string-match "/app/controllers/" path))
(defun rails-api-file-p (path) (string-match "/app/apis/" path))

(defun rails-model-file (path)
  (replace-regexp-in-string "/test/unit/\\(.*\\)\\(test\\|spec\\)\\.rb"
			    "/app/models/\\1.rb" path))

(defun rails-unit-test-file (path)
  (replace-regexp-in-string "/app/models/\\(.*\\)\\.rb"
			    "/test/unit/\\1_test.rb" path)
;;   (replace-regexp-in-string "/test/fixtures/\\(.*\\)s\\.rb"
;; 			    "/test/unit/\\1_test.rb" path)
)

(defun rails-functional-test-file (path)
  (replace-regexp-in-string "/app/controllers/\\(.*\\)\\.rb"
			    "/test/functional/\\1_test.rb" path))

(defun rails-controller-file (path)
  (replace-regexp-in-string "/test/functional/\\(.*\\)_\\(test\\|spec\\)\\.rb"
			    "/app/controllers/\\1.rb" path))

(defun rails-controller-from-view-file (path)
  (replace-regexp-in-string "/app/views/\\(.*\\)/[^/].*"
			    "/app/controllers/\\1_controller.rb" path))

(provide 'rails-find-other-file)
