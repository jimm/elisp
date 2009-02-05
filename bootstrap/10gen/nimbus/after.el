(add-to-list 'auto-mode-alist '("\\.xson$" . xml-mode))

(defun zoom-frame-width-cols ()
  (interactive)				; for testing
  268)                                  ; 268 for really full screen, 260 with dock exposed

(defun my-visit-work-todo () (interactive) (org-open-file "~/pim/orgs/10gen.org" t nil "* To Do"))
(defun my-visit-home-todo () (interactive) (find-file (concat *my-pim-dir* "orgs/todo.org")))
(defun my-visit-home-notes-page () (interactive) (find-file *my-remember-data-file*))
; (defun my-visit-work-notes-page () (interactive) (emacs-wiki-find-file "NotesPage"))
(defun my-visit-work-notes-page () (interactive) (org-open-file "~/pim/orgs/10gen.org" t nil "* Notes"))

(defun my-run-framework-testcase ()
  (interactive)
  (compile (concat "ant -e -s build.xml -Dtestclass="
		   (path-to-java-package (buffer-file-name)) "."
		   (file-name-sans-extension (file-name-nondirectory
					      (buffer-file-name)))
		   " test-one")))

(add-hook 'java-mode-hook
	  '(lambda ()
	     (define-key java-mode-map "\C-cr" 'my-run-framework-testcase)
	     (define-key java-mode-map "\C-c\C-r" 'my-run-framework-testcase)))

;; (setq sql-user "jimm")
;; (setq sql-server "localhost")
;; (setq sql-database "db")

(setq browse-url-generic-program "open")
(setq Man-switches "-M /usr/share/man:/usr/local/share/man")

(defvar compile-ant "ant -e -s build.xml ")
; note: testng must be run from ed directory
(defvar compile-test-ruby "cd ~/src/ed && ant -e -Druby.groups=ruby,ruby.db,ruby.activerecord test-ruby")
(defvar run-jruby-test "cd ~/src/sandbox/jruby && make")

(set-register ?n compile-ant)
(set-register ?r "cd ~/src/ed && ant -e -Druby.groups=ruby.activerecord test-ruby")
(set-register ?t compile-test-ruby)
(set-register ?u "cd ~/src/ed && ant -e -Druby.groups=ruby.testunit test-ruby")
(set-register ?v "cd ~/src/ed && ant -e -Druby.groups=ruby.testunit,ruby.activerecord test-ruby")
(set-register ?j run-jruby-test)
(set-register ?q "rake test && /usr/local/ruby19/bin/rake test")
(setq compile-command (get-register ?q))

(custom-set-variables
 '(pmd-java-home "/usr/bin/java"))

;; (defun my-run-framework-testcase ()
;;   (interactive)
;;   (compile (concat "cd ~/src/ed && ant -e && ./runLight.bash "
;; 		   (my-java-read-package) "."
;; 		   (file-name-sans-extension (file-name-nondirectory
;; 					      (buffer-file-name))))))
(defun my-run-framework-testcase ()
  (interactive)
  (compile compile-testng-ruby))

(add-hook 'java-mode-hook
	  '(lambda ()
	     (define-key java-mode-map "\C-cr" 'my-run-framework-testcase)
	     (define-key java-mode-map "\C-c\C-r" 'my-run-framework-testcase)))

(server-start)

(setf tags-file-name "/Users/jimm/src/ed/TAGS")

(defun my-send-du-email ()
  "Opens mail client with an email containing today's du.org
entry. See todays-du."
  (interactive)
  (shell-command (concat "open " (shell-quote-argument (concat "mailto:dev@10gen.com?subject=du&body=" (url-hexify-string (du-for-date)))))))

(defun du-for-date ()
  "Returns today's du.org entry"
  (interactive)
  (save-excursion
    (org-open-file "~/Documents/du.org" t nil (concat "* <" (org-read-date)))
    (org-copy-subtree)
    (substring-no-properties (current-kill 0 t))))

(define-key global-map [f4] 'my-visit-work-todo)
(define-key global-map [\C-f4] 'my-visit-home-todo)
(define-key global-map [f5] 'git-status)
(define-key global-map [f6] 'my-visit-work-notes-page)
(define-key global-map [\C-f6] 'my-visit-home-notes-page)
(define-key global-map [f7]
  (lambda () (interactive) (switch-to-buffer "*git-status*")))
