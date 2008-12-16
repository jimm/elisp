;; default-directory is a variable that holds name of directory of current
;; buffer.

(defun rails-root-dir-p (dir)
  "Returns t if dir is a Rails root directory."
  (interactive "D")
  (and (file-exists-p dir)
       (file-directory-p dir)
       (file-exists-p (concat dir "Rakefile"))
       (file-exists-p (concat dir "app"))
       (file-exists-p (concat dir "config"))
       (file-exists-p (concat dir "log"))
       (file-exists-p (concat dir "public"))
       (file-exists-p (concat dir "script"))))

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
;;   '(defun ,(concat "rails-" which "-dir") (path)
;;      (concat (rails-root-dir path) "app/" ,which "/")))

(defun rails-controllers-dir (path)
  (concat (rails-root-dir path) "app/controllers/"))
(defun rails-models-dir (path)
  (concat (rails-root-dir path) "app/models/"))
(defun rails-views-dir (path)
  (concat (rails-root-dir path) "app/views/"))
(defun rails-helpers-dir (path)
  (concat (rails-root-dir path) "app/helpers/"))
(defun rails-test-dir (path)
  (concat (rails-root-dir path) "test/"))
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

(defun rails-find-other-file ()
  "Finds the \"other\" file, \"other\" being defined depending upon the
current buffer."
  (interactive)
  (let ((path (buffer-file-name)))
    (find-file
     (cond ((rails-unit-test-file-p path) (rails-model-file path))
	   ((rails-fixture-p path) (rails-unit-test-file path))
	   ((rails-model-file-p path) (rails-unit-test-file path))
	   ((rails-api-test-file-p path) (rails-controller-file path))
	   ((rails-functional-test-file-p path) (rails-controller-file path))
	   ((rails-controller-file-p path) (rails-functional-test-file path))
	   ((rails-api-file-p path) (rails-api-test-file path))
	   ((rails-view-file-p path) (rails-controller-from-view-file path))))))

(defun rails-unit-test-file-p (path) (string-match "/test/unit/" path))

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
  (replace-regexp-in-string "/test/unit/\\(.*\\)_test\\.rb"
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

(defun rails-api-file (path)
  (replace-regexp-in-string "/test/functional/\\(.*\\)_test\\.rb"
			    "/app/apis/\\1.rb" path))

(defun rails-api-test-file (path)
  (replace-regexp-in-string "/app/apis/\\(.*\\)\\.rb"
			    "/test/functional/\\1_test.rb" path))

(defun rails-controller-file (path)
  (replace-regexp-in-string "/test/functional/\\(.*\\)_test\\.rb"
			    "/app/controllers/\\1.rb" path))

(defun rails-controller-from-view-file (path)
  (replace-regexp-in-string "/app/views/\\(.*\\)/[^/].*"
			    "/app/controllers/\\1_controller.rb" path))

(provide 'rails-find-other-file)
