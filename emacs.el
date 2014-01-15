;; -*- emacs-lisp -*-

;;; Note: this file should be loaded by bootstrap-init.el.

(defmacro when-fboundp-call (f &rest args)
  "If F is bound, calls it with ARGS."
  `(when (fboundp (quote ,f)) (funcall (quote ,f) ,@args)))

(defmacro unless-boundp-setq (var val)
  "If VAR is not bound, sets it to VAL."
  `(unless (boundp (quote ,var)) (setq ,var ,val)))

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(when (< emacs-major-version 24)
  (progn
    ;; This was installed by package-install.el.
    ;; This provides support for the package system and
    ;; interfacing with ELPA, the package archive.
    ;; Move this code earlier if you want to reference
    ;; packages in your .emacs.
    (let ((f (expand-file-name "~/.emacs.d/elpa/package.el")))
      (when (and (file-exists-p f)
                 (load f))))))
(when-fboundp-call package-initialize)

(defun ensure-ends-with-slash (dir)
  "If DIR does not end with \"/\", return a new copy of DIR that
  does."
  (if (equal "/" (substring dir -1)) dir
    (concat dir "/")))

(defvar aquamacs-p (string-match "Aquamacs" (version)))
;; The following method works and will be supported in the future:
;;  (when (boundp 'aquamacs-version)
;;    ... Aquamacs specific code ...
;;  )
;; - DavidReitter

(defvar my-shell 'eshell
  "The shell to use inside Emacs; examples include 'shell or 'eshell.")

(setq eshell-directory-name (concat *my-emacs-lib-dir* "eshell/"))

(if (equal default-directory "/") (setq default-directory "~/"))

; Add *my-emacs-lib-dir* subdirs to the end of load-path, so if it's
; pre-installed that version is used first.
(mapc (lambda (dir)
        (add-to-list 'load-path (concat *my-emacs-lib-dir* dir "/") t))
      '("progmodes" "ses" "remember" "org/lisp"))
; Don't need any org mode contributions yet
; (add-to-list 'load-path (concat *my-emacs-lib-dir* "org/contrib/lisp/") t)

(setq abbrev-file-name (concat *my-emacs-lib-dir* "abbrev_defs.el"))
(read-abbrev-file abbrev-file-name t)
(load "my-skeletons")

;; IDO mode
(load-library "ido")
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'generic-x); DOS batch, ini files and much more
(add-to-list 'auto-mode-alist
             '("\\.properties$" . java-properties-generic-mode))

;;
;; Global variable settings and options
;;
(when-fboundp-call appt-activate 1)	; appointment notification
(when-fboundp-call line-number-mode -1) ; don't display 'em

;; Ubuntu stuff
;(menu-bar-enable-clipboard)
(setq x-select-enable-clipboard t)
(setq mouse-drag-copy-region t)
(when-fboundp-call set-scroll-bar-mode 'right)
(setq skeleton-pair nil)
(mouse-wheel-mode 1)

;; Emacs 23-specific values
(when (>= emacs-major-version 23)
  (progn
    (transient-mark-mode -1)
    (setq confirm-nonexistent-file-or-buffer nil)))

(setq bookmark-save-flag 1)		; see bootstrap-ini for loc of file
(setq sentence-end-double-space nil)
(turn-on-auto-fill)
(auto-fill-mode 1)
(show-paren-mode 1)
(setq column-number-mode nil)
(setq-default fill-column 76)
(setq make-backup-files nil)            ; don't make backup files
(setq delete-auto-save-files t)         ; no "#" files after a save
(setq auto-save-list-file-prefix nil)   ; don't record sessions
(setq inhibit-startup-screen t)         ; kill the startup message
(setq initial-scratch-message nil)      ; used by Emacs 23 and above
(setq inhibit-startup-echo-area-message "jimm")
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq compile-command "makeup ")        ; script finds make/rake/pom/build
(setq Man-notify 'aggressive)           ; when man found, jump there *immed*
(setq dabbrev-case-replace nil)         ; preserve case when expanding
(setq grep-command "grep -n ")
(setq mode-require-final-newline nil)	; do not force newlines

(fset 'yes-or-no-p 'y-or-n-p)           ; accept simple 'y'/space, 'n'/delete
(unless (fboundp 'string-match-p) (defalias 'string-match-p 'string-match))

;; Build a custom grep-find-command
(unless-boundp-setq *more-grep-find-bad-names* ())
(let* ((more-bad-names (or *more-grep-find-bad-names* ()))
       (bad-names (append (list "*.log" ".svn" ".git" "CVS" "TAGS" "*~" "*.class"
                                "*.[wj]ar" "target" "javadoc" "bytecode" "*.beam"
                                "*.swf" "*.o" "_site" "*.pyc")
                          more-bad-names))
       (gfc (concat "find . \\( -name "
                    (mapconcat 'shell-quote-argument bad-names " -o -name ")
                    " \\) -prune -o -type f -print0 | xargs -0 grep -H -n ")))
  (setq grep-find-command (cons gfc (+ 1 (length gfc)))))

(setq visible-bell t)
(setq version-control 'never)           ; When to make backup files
(setq vc-handled-backends '())          ; disable VC minor mode
(setq frame-title-format "%b - Emacs")

(when (functionp 'tool-bar-mode) (tool-bar-mode -1))
(unless window-system (menu-bar-mode nil))

(global-font-lock-mode t)               ; always turn on, where available

(defun underscores-to-camel-case (str)
  "Converts STR, which is a word using underscores, to camel case."
  (interactive "S")
  (apply 'concat (mapcar 'capitalize (split-string str "_"))))

;; I prefer zap-upto-char most of the time
(defun zap-upto-char (arg char)
  "Kill up to but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (read-char "Zap upto char: " t)))
  (zap-to-char arg char)
  (insert-char char)
  (backward-char))

;;
;; Display time and add time-related hooks
;;
;; (display-time)                       ; display time and "Mail" in mode lines

(defun generate-random-sig ()
  (interactive)
  (shell-command "random_sig.rb"))
;;; The following code randomly generates a sig every N seconds. Now a days,
;;; a cron job takes care of this.
;; (load "timer")
;; (run-at-time t 300 'generate-random-sig)

;;;
;;; YAML-mode
;;;
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;;
;; For flipping back and forth between files and headers, or Java/Ruby files
;; and Java/Ruby test files.
;;
(defun my-ff-find-other-file (&optional in-other-window ignore-include)
  "Find other Java or Ruby file or, if not a Java or Ruby file,
call `ff-find-other-file`."
  (interactive "P")
  (let* ((fname (buffer-file-name))
	 (ext (file-name-extension fname)))
    (cond ((or (equal "java" ext) (equal "scala" ext))
	   (find-other-java-file fname ext))
	  ((equal "rb" ext)
	   (find-other-ruby-file fname))
	  (t
	   (ff-find-other-file in-other-window ignore-include)))))

(defun find-other-java-file (&optional file-name ext)
  "Visits `Foo.java' when given `FooTest.java' and vice versa.
Default file-name is current buffer's name."
  (interactive)
  (let* ((fname (file-name-nondirectory (or file-name (buffer-file-name))))
         (non-test-from-end (- -5 (length ext)))
         (test-from-end (- -1 (length ext)))
	 (target (if (equal (concat "Test." ext) (substring fname non-test-from-end))
		     (concat (substring fname 0 non-test-from-end) (concat "." ext))
		   (concat (substring fname 0 test-from-end) (concat "Test." ext)))))
    (ef target default-directory)))

(defun find-other-ruby-file (&optional file-name)
  "Visits `foo.rb' when given `foo_(test|spec).rb'. Visits `foo_test.rb' when
given `foo.rb'. Default file-name is current buffer's name."
  (interactive)
  (let* ((fname (file-name-nondirectory (or file-name (buffer-file-name))))
	 (target (if (and (> (length fname) 8)
                          (or (equal "_test.rb" (substring fname -8))
                              (equal "_spec.rb" (substring fname -8))))
                     (concat (substring fname 0 -8) ".rb")
 		   (concat (substring fname 0 -3) "_test.rb"))))
    (ef target default-directory)))

(setq ff-other-file-alist
      '(("\\.cpp$" . ((".hpp" ".hh" ".h")))
        ("\\.cp$" . ((".hp" ".hh" ".h")))
        ("\\.hpp$" . ((".cpp" ".cp")))
        ("\\.icc$" . ((".hpp")))
        ("\\.m$" .  ((".h")))
        ("\\.cc$" .  ((".hh" ".h")))
        ("\\.hh$" .  ((".cc" ".C" ".CC" ".cxx" ".cpp" ".cp" ".m")))
        ("\\.c$" .  ((".h")))
        ("\\.h$" .  ((".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".cp" ".m")))
        ("\\.C$" .  ((".H" ".hh" ".h")))
        ("\\.H$" .  ((".C" ".CC")))
        ("\\.CC$" .  ((".HH" ".H" ".hh" ".h")))
        ("\\.HH$" .  ((".CC" ".C")))
        ("\\.cxx$" .  ((".hh" ".h" ".hxx")))
      ))

;; When saving files, set execute permission if #! is in first line.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(defun capitalize-next-char ()
  "Capitalize next character and move point right 1 character."
  (interactive "*")
  (save-excursion
    (forward-char)
    (insert " ")
    (backward-char 2)
    (capitalize-word 1)
    (delete-char 1)))

(defun capitalize-first-char-of-string (str)
  "Capitalize first character of STR."
  (interactive "s")
  (concat (capitalize (substring str 0 1)) (substring str 1)))

(defun downcase-first-char-of-string (str)
  "Make first character of STR lower-case."
  (interactive "s")
  (concat (downcase (substring str 0 1)) (substring str 1)))

(defun pluralize (str)
  "Pluralize STR, which is assumed to be a single word. This is a
simple algorithm that may grow over time if needed."
  (interactive "s")
  (let ((len (length str)))
    (cond ((equal "y" (substring str (- len 1))) (concat (substring str 0 (- len 1)) "ies"))
          ((equal "us" (substring str (- len 2))) (concat (substring str 0 (- len 2)) "i"))
          (t (concat str "s")))))

(defun singularize (str)
  "Singularize STR, which is assumed to be a single word. This is
a simple algorithm that may grow over time if needed."
  (interactive "s")
  (let ((len (length str)))
    (cond ((equal "ies" (substring str (- len 3))) (concat (substring str 0 (- len 3)) "y"))
          ((equal "i" (substring str (- len 1))) (concat (substring str 0 (- len 1)) "us"))
          ((equal "s" (substring str (- len 1))) (substring str 0 (- len 1)))
          (t str))))

(defun debug-comment ()
  "Add a DEBUG comment to the current line"
  (interactive "*")
  (save-excursion
    (comment-dwim nil)
    (insert " DEBUG")))

;;
;; PIM
;;
(setq diary-file (concat *my-pim-dir* "diary")) ; must be before remember mode hook def

;;
;; Remember mode
;;
(autoload 'remember "remember" nil t)
(setq *my-remember-data-file* (concat *my-pim-dir* "orgs/notes.org"))
(add-hook 'remember-mode-hook
          '(lambda ()
             (setq remember-data-file *my-remember-data-file*)
             (setq remember-diary-file diary-file)))

;;
;; Browse away!
;;
;(autoload 'browse-url-netscape "browse-url" "Ask Netscape to show a URL" t)
(defvar shell-open-file-list '("\\.html$" "\\.pdf$" "\\.app$" "\\.rtfd?$" "\\.docx?$"))

(defun matches-regexp-in-list-p (str list)
  (cond ((null list) nil)
        ((string-match (car list) str) t)
        (t (matches-regexp-in-list-p str (cdr list)))))

(defun my-goto-calendar-date (str)
  (interactive "sDate: ")
  (let ((date-list (cdddr (parse-time-string str))))
    (calendar)
    (calendar-goto-date (list (cadr date-list)
			      (car date-list)
			      (caddr date-list)))))

(defun open-url-using-emacs-p (str)
  (let ((len (length str)))
    (and (string-prefix-p "file:" str)
         (not (matches-regexp-in-list-p str shell-open-file-list)))))

;(setq browse-url-generic-program "mozilla-firefox")
(setq browse-url-generic-program "open")

(defun my-url-open (&optional url unused)
  "UNUSED is there because org-open-at-point demands it."
  (interactive)
  (let* ((url-str (if (null url) (read-string "URL: ") url)))
    (cond ((open-url-using-emacs-p url-str)
           (find-file (substring url-str ; chop off "file://" or "file:" first
                                 (if (equal (string-match "file://" url-str) 0)
                                     7 5))))
          ((string-prefix-p "addr:" url-str)
           (address (substring url-str 5)))
          ((string-prefix-p "date:" url-str)
           (my-goto-calendar-date (substring url-str 5)))
          ((and (not (string-prefix-p "http://" url-str)) (not (string-prefix-p "https://" url-str)))
           (browse-url-generic (concat "http://" url-str)))
          (t
           (browse-url-generic url-str)))))

(setq browse-url-browser-function 'my-url-open)

; Java class Javadoc lookup
(defvar *my-javadoc-url*
  "http://download.oracle.com/javase/1.5.0/docs/api/"
  "The start of the URL to use to open Java API docs. Override this when
you have a local copy, for example.")

(defun my-javadoc-open (&optional klass)
  (interactive)
  (let* ((klass-str (if (null klass) (read-string "Full class name: ") klass))
         (klass-path (replace-regexp-in-string "\\." "/" klass-str))
         (url (concat *my-javadoc-url* klass-path ".html")))
    (browse-url-generic url)))

;;; ================================================================
;;; Finding files
;;; ================================================================

;;
;; ef (find)
;; must come before loading my eshell customize
;;
(load "find-lisp")
(defun ef (fname-regexp root-directory)
  "Searches recursively in ROOT-DIRECTORY or current directory
for FNAME-REGEXP. If one file is found, that file is opened. If
more than one id found, opens a dired buffer on the list of
files. If no files are found, continue searching up the directory
tree.

Ignores certain directories such as .git, .svn, and target. This
list is hard-coded, though it would be easy to make it an
optional argument."
  (interactive "sFilename regex: \nDSearch root directory: ")
  (let* ((dir (file-name-as-directory root-directory))
	 (dirname (directory-file-name dir))
	 (files
	  (cl-remove-if
	   (lambda (f) (string-match
                        "^\\(\\.(git\\|svn)\\)\\|\\(classes\\|build\\|target\\|CVS\\)$\\|^~"
                        f))
	   (split-string (shell-command-to-string
			  (concat "find " dirname " -name " fname-regexp)))))
	 (len (length files)))
    (cond ((zerop len)
	   (cond ((file-system-root-dir-p dirname) (message "%s not found" fname-regexp))
		 (t (ef fname-regexp (file-name-directory dirname)))))
	  ((= 1 len) (find-file (car files)))
	  (t (find-dired dir (concat "-name " fname-regexp))))))

;; Returns file(s) starting at default directory. See also
;; get-closest-pathname defined below (which does the same thing but assumes
;; root-directory is default-directory).
(defun find-up (fname-regexp root-directory)
  "Searches starting at ROOT-DIRECTORY for FNAME-REGEXP. If not
found, goes up until it hits the system root directory, looking
for FNAME-REGEXP."
  (interactive "sFilename regex: \nDSearch root directory: ")
  (let* ((dir (file-name-as-directory (expand-file-name root-directory)))
	 (dirname (directory-file-name dir))
	 (filter-regexp "\\.git\\|\\.svn\\|classes\\|build\\|target\\|TAGS\\|CVS\\|~")
	 (files
	  (remove-if
	   (lambda (f) (string-match filter-regexp f))
           (directory-files dirname t fname-regexp)))
	 (len (length files)))
    (cond ((zerop len)
	   ; (message "%s not found in %s" fname-regexp dir))
	   (cond ((file-system-root-dir-p dirname) (message "%s not found" fname-regexp))
		 (t (find-up fname-regexp (file-name-directory dirname)))))
	  ((= 1 len) (car files))
	  (t files))))

;; See also find-up and the built-in function locate-dominating-file.
(defun get-closest-pathname (file)
  "Determine the pathname of the first instance of FILE starting from the
current directory towards root. This may not do the correct thing in presence
of links. If it does not find FILE, then it shall return the name of FILE in
the current directory, suitable for creation"
  (let* ((path (expand-file-name file))
         (f (file-name-nondirectory path))
         (d (locate-dominating-file path f)))
    (if d (concat d f)
      (expand-file-name f default-directory))))

;;; ================================================================

;;
;; Shell script
;;
(defun run-sh-buffer ()
  (interactive)
  (let ((fn (buffer-file-name)))
    (if (file-exists-p fn)
        (progn
          (save-buffer)
          (compile (buffer-file-name)))
      (progn
        (let ((tmpfile (make-temp-file "sh-region-" nil ".sh")))
          (write-region nil nil tmpfile)
          (compile tmpfile))))))
;; Can't remove temp file because compile is async and file could be deleted
;; before it is used by compile.

(add-hook 'sh-mode-hook
          '(lambda ()
             (define-key sh-mode-map "\C-cr" 'run-sh-buffer)))

;;
;; Subversion
;;
(autoload 'svn-status "psvn")

;;
;; Tramp
;;
; (setq tramp-default-method "scp")
(defun debug-tramp ()
  (interactive)
  (setq tramp-debug-buffer t)
  (setq tramp-verbose 10))

;;
;; ssh-ing
;;
(defun ssh (host)
  "Open a shell on a buffer named *HOST* and run ssh HOST. Note that HOST can
  be of the form USER@HOST."
  (interactive "sHost: ")
  (shell (concat "*" host "*"))
  (insert (concat "ssh " host))
  (comint-send-input))

;;
;; Text-mode
;;
(add-hook 'text-mode-hook
          '(lambda ()
             (auto-fill-mode 1)
             (four-tab-stops)))

;;
;; For both C and C++ mode
;;
(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq c-basic-offset 2)
             (setq c-tab-always-indent nil)
             ;; BAD! BAD! Screws up ^D
             ;; (setq c-delete-function 'backward-delete-char)
             (setq c-recognize-knr-p nil)

             ;; (define-key c-mode-map "{" 'skeleton-pair-insert-maybe)
             ;; (define-key c-mode-map "(" 'skeleton-pair-insert-maybe)

             ;; (local-set-key "\M-o" 'fh-open-header-file-other-window)
             ;; (local-set-key "\M-O" 'fh-open-header-file-other-frame)
             (local-set-key "\r" 'newline-and-indent)
             (autoload 'fh-open-header-file-other-window "find-header"
               "Locate header file and load it into other window" t)
             (autoload 'fh-open-header-file-other-frame "find-header"
               "Locate header file and load it into other frame" t)))

;;
;; C++-mode
;;
(add-to-list 'auto-mode-alist '("\\.[ch]pp?$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h?$" . c++-mode)) ; mostly C++, now a days
(add-to-list 'auto-mode-alist '("\\.pde$" . c++-mode)) ; Arduino
(add-hook 'c++-mode-hook
          '(lambda ()
             (c-set-style "stroustrup")
             (setq c-basic-offset 2)))

;;
;; Java-mode
;;
(eval-after-load "java"
  (load "my-java-mode"))

(add-to-list 'auto-mode-alist '("\\.aj$" . java-mode)) ; Roo aspect files
(add-to-list 'auto-mode-alist '("\\.jsp$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.w[as]r$" . archive-mode))
(add-hook 'java-mode-hook
          '(lambda ()
             (c-set-style "java")
             ;; (c-set-offset 'inclass 0)
             (if window-system (font-lock-mode 1))
             ;; (define-key java-mode-map "{" 'skeleton-pair-insert-maybe)
             ;; (define-key java-mode-map "(" 'skeleton-pair-insert-maybe)
             (define-key java-mode-map "\C-cp" 'my-insert-println)
             (define-key java-mode-map "\C-ce" 'my-insert-err-println)
             (define-key java-mode-map "\C-cd" 'my-insert-debug-println)))

;;;
;; Compilation mode
;;
(require 'compile)
; Maven 2 error messages are of the form file:[line,column]
(setq compilation-error-regexp-alist
      (cons
       '("^\\(/[^:]+\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 1 2 3)
       compilation-error-regexp-alist))
; Scala error messages
(setq compilation-error-regexp-alist
      (cons
       '("\\(\\([a-zA-Z0-9]*/\\)*\\([a-zA-Z0-9]*\\.scala\\)\\):\\([0-9]*\\).*" 1 2)
       compilation-error-regexp-alist))
;;
;; YASnippet
;;

(when (fboundp 'yas-global-mode)
  (yas-global-mode 1)
  ; Don't do the following; link ~/.emacs.d/snippets to snippets instead
  ; (add-to-list 'yas-snippet-dirs (concat *my-emacs-lib-dir* "snippets"))
  (yas-reload-all))

;;
;; JavaScript
;;;
(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist '("\\.[agj]s$" . javascript-mode))
(add-hook 'js-mode-hook
          '(lambda ()
             (setq js-indent-level 2)   ; need both?????
             (setq javascript-indent-level 2)))
;; (autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq js2-basic-offset 2)
;(setq js2-use-font-lock-faces t)

;;
;; CoffeeScript
;;
(defun run-coffee-buffer ()
  (interactive)
  (compile (concat "coffee " (shell-quote-argument (buffer-file-name)))))

(defun compile-coffee-buffer ()
  (interactive)
  (shell-command (concat "coffee"
                         " -o " (shell-quote-argument (file-name-directory (buffer-file-name)))
                         " -c " (shell-quote-argument (buffer-file-name)))))

(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(add-hook 'coffee-mode-hook
          '(lambda ()
             (setq coffee-js-mode 'javascript-mode)
             (define-key coffee-mode-map "\C-cr" 'run-coffee-buffer)
             (define-key coffee-mode-map "\C-ck" 'compile-coffee-buffer)
             (set (make-local-variable 'tab-width) 2)))

;;
;; Objective-C mode
;;
(add-hook 'objc-mode-hook
          '(lambda ()
             (if window-system (font-lock-mode 1))
             (setq c-basic-offset 4)))

;;
;; Groovy mode
;;
(autoload 'groovy-mode "groovy-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))
(defun run-groovy-buffer ()
  (interactive)
  (save-buffer)
  (compile (concat "groovy " (buffer-file-name))))
(add-hook 'groovy-mode-hook
          '(lambda ()
             (setq groovy-basic-offset 4)
             (define-key groovy-mode-map "\r" 'newline-and-indent)
             (define-key groovy-mode-map "\C-cr" 'run-groovy-buffer)
             (font-lock-mode 1)))

;; Groovy shell mode
(autoload 'run-groovy "inf-groovy" "Run an inferior Groovy shell process")
(autoload 'inf-groovy-keys "inf-groovy"
  "Set local key defs for inf-groovy in groovy-mode")
(add-hook 'groovy-mode-hook
          '(lambda ()
             (inf-groovy-keys)))

;;
;; Scheme mode
;;
(add-hook 'scheme-mode-hook
          '(lambda ()
             (define-key scheme-mode-map "\r" 'newline-and-indent)
             (define-key scheme-mode-map "\C-cd" 'debug-comment)))

;;
;; Perl-mode
;;
(autoload 'perl-mode "perl-mode" "Perl mode" t nil)
(add-hook 'perl-mode-hook
          '(lambda ()
             (define-key perl-mode-map "\r" 'newline-and-indent)
             (define-key perl-mode-map "\M-\C-h" 'backward-kill-word)
             (define-key perl-mode-map "\C-cd" 'debug-comment)
             (setq c-basic-offset 2)
             (setq c-tab-always-indent nil)))

;;
;; sh-mode
;;
(add-hook 'sh-mode-hook
          '(lambda ()
             (define-key sh-mode-map "\C-c\C-k" 'compile)))

;;
;; Eshell-mode
;; must come after defining ef
;;
(if (eq my-shell 'eshell)
    (progn
      (load "eshell")
      (load "eshell-customize")

      (unless aquamacs-p
        ; Mac OS X doesn't set path properly when Emacs.app is launched.
        ; Since Mac OS X is pretty much all I use these days, I've put this
        ; code here. Shouldn't do any harm if run on another flavor of Unix.
        (when (file-exists-p "/bin/bash")
          ;; Launch subshell and pring env vars, then parse that and set our
          ;; env vars.
          (let ((all-env-vars (shell-command-to-string "/bin/bash -l -c '/usr/bin/env'")))
            (mapc (lambda (line)
                    (when (string-match "\\([^=]+\\)=\\(.*\\)" line)
                      (setenv (match-string 1 line) (match-string 2 line))))
                  (split-string all-env-vars "\n")))

          (let ((path (getenv "PATH")))
            (setq eshell-path-env path)
            (setq exec-path (split-string path ":")))))))

;;
;; Shell-mode
;;

;; Don't echo passwords
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)
(setq shell-completion-execonly nil)    ; Any file is completion candidate
(add-hook 'shell-mode-hook
          '(lambda ()
             (auto-fill-mode -1)
             (setq comint-scroll-show-maximum-output nil)))

;;
;; LaTeX-mode
;;
(eval-after-load "latex-mode"
  (progn
    (setq tex-dvi-view-command "latex-view '*'") ; in my ~/bin dir
    ;; (setq tex-dvi-view-command "xdvi -expert -hush")
    (setq tex-dvi-print-command "dvips -f * | lpr")

    (defun my-tex-to-text ()
      (interactive)
      (save-buffer)
      (shell-command (concat
                      "/usr/bin/dvi2tty "
                      (file-name-sans-extension (buffer-file-name))
                      " >"
                      (file-name-sans-extension (buffer-file-name))
                      ".txt"))
      )
    (add-hook 'tex-mode-hook
              '(lambda () (define-key tex-mode-map "\C-c\C-k" 'compile)))
    (add-hook 'latex-mode-hook
              '(lambda ()
                 (define-key latex-mode-map "\C-c\C-p" 'tex-print)
                 ;; (define-key latex-mode-map "\C-c\C-t" 'my-tex-to-text)
                 (define-key latex-mode-map "\C-c\C-k" 'compile)
                 (define-key latex-mode-map "\C-c\C-i" 'find-mine)
                 (define-key latex-mode-map "\C-c\C-s" 'my-tex-slide-dvi-view)))))

;;
;; Sql-mode
;;
(eval-after-load "sql-mode"
  (load "my-sql-mode"))

;;
;; Clojure-mode (lisp)
;;

(eval-after-load "clojure-mode"
  (load "my-clojure-mode"))

;;
;; Lisp-mode and slime-mode
;;
;; Now that I'm using Clojure, I don't want sbcl to be the default program.
;;
;; Note: use nREPL instead by running (M-x nrepl-jack-in) to start lein nrepl.
(setq inferior-lisp-program "lein repl")
(defun clj ()
  (interactive)
  (setq inferior-lisp-program "clj -n")
  (inferior-lisp "clj -n"))
(defun lein-repl ()
  (interactive)
  (setq inferior-lisp-program "lein repl")
  (inferior-lisp "lein repl"))

;; (require 'slime)
;; (slime-setup)

(add-hook 'lisp-mode-hook
          '(lambda ()
             (define-key lisp-mode-map "\r" 'newline-and-indent)
             (define-key lisp-mode-map "\C-cd" 'debug-comment)))

;;
;; Clisp
;;
(defun clisp ()
  (interactive)
  (setq inferior-lisp-program "clisp")
  (inferior-lisp "clisp"))

;;
;; SBCL
;;
(defun sbcl ()
  (interactive)
  (setq inferior-lisp-program "sbcl")
  (inferior-lisp "sbcl"))

;;
;; Emacs-Lisp-mode
;;
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (define-key emacs-lisp-mode-map "\C-cd" 'debug-comment)
             (define-key emacs-lisp-mode-map "\r" 'newline-and-indent)))

;;
;; PHP-mode
;;
(autoload 'php-mode "php-mode" "PHP mode" t nil)
(add-to-list 'auto-mode-alist '("\\.\\(php\\|inc\\)$" . php-mode))
(eval-after-load "php-mode"
  (progn
    (defun run-php-buffer ()
      (interactive)
      (save-buffer)
      (compile (concat "php -f " (buffer-file-name))))
    (add-hook 'php-mode-hook
              '(lambda ()
                 (auto-fill-mode 1)
                 (setq c-basic-offset 4)
                 (define-key php-mode-map "\C-d" 'delete-char)
                 (define-key php-mode-map "\C-ct" 'html-mode)
                 (define-key php-mode-map "\C-ch" 'insert-ruby-hash-arrow)
                 (define-key php-mode-map "\C-cr" 'run-php-buffer)))))

;;
;; HTML-mode and SGML-mode
;;
(eval-after-load "html-mode"
  (progn
    (defun my-html-insert-comment ()
      (interactive "*")
      (insert "<!--  -->")
      (backward-char 4)
      )

    (defun unescape-html ()
      (interactive "*")
      (let ((repl-alist '(("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"")
                          ("&apos;" . "'") ("&amp;" . "&"))))
        (save-excursion
          (mapcar (lambda (alist)
                    (goto-char (point-min))
                    (while (re-search-forward (car alist) nil t)
                      (replace-match (cdr alist) nil nil)))
                  repl-alist))))))

(mapcar (lambda (ext)
          (add-to-list 'auto-mode-alist
                       (cons (concat "\\." ext "$") 'sgml-mode)))
        ; jwcs, application, page: Tapestry
        ; ftl: FreeMarker
        '("xsd" "wsd[ld]" "jwcs" "application" "page" "ftl"))

(eval-after-load "sgml-mode"
  (progn
    (add-hook 'sgml-mode-hook
              '(lambda ()
                 (require 'tex-mode)
                 (auto-fill-mode 1)
                 (define-key sgml-mode-map "\C-c\C-k" 'compile)))

    (add-hook 'html-mode-hook
              '(lambda ()
                 (auto-fill-mode 1)
                 (define-key html-mode-map "\C-c;" 'my-html-insert-comment)
                 (define-key html-mode-map "\C-cp" 'php-mode)
                 ;; The remaining functions are defined in my-ruby-mode.el
                 (define-key html-mode-map "\C-ce" 'erb-eval-skeleton)
                 (define-key html-mode-map "\C-cp" 'erb-print-skeleton)
                 (define-key html-mode-map "\C-ch" 'insert-ruby-hash-arrow)
                 (define-key html-mode-map "\C-cl" 'rails-link-to-skeleton)
                 (define-key html-mode-map "\C-cr" 'rails-render-partial-skeleton)))))

;;
;; CSS-mode
;;
(autoload 'css-mode "css-mode" "CSS mode" t nil)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

;;
;; Ruby-mode
;;
;; Use "M-x run-ruby" to start inf-ruby.
(autoload 'ruby-mode "ruby-mode" "Ruby mode" t nil)
(autoload 'run-ruby "inf-ruby" "Ruby inferior process (irb)" t nil)

(defalias  'inf-ruby 'run-ruby)
(defalias  'inferior-ruby 'run-ruby)
(defalias 'irb 'run-ruby)

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.r\\(b\\(w\\|x\\)?\\|html?\\|js\\)$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\([Rr]ake\\|[Cc]ap\\|[Gg]em\\)file$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gem\\(spec\\)?$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.duby$" . ruby-mode))

(eval-after-load "ruby-mode"
  (load "my-ruby-mode"))
(eval-after-load "inf-ruby"
  (load "my-ruby-mode"))

;;
;; Erlang-mode
;;
(when (and (boundp '*my-erlang-emacs-tools-dir*)
	   (file-exists-p *my-erlang-emacs-tools-dir*))
  (add-to-list 'load-path *my-erlang-emacs-tools-dir* t))
(autoload 'erlang-mode "erlang" "Erlang mode" t nil)
(add-to-list 'auto-mode-alist '("\\.[he]rl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.yaws$" . erlang-mode))
(add-hook 'erlang-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (comment-set-column 32)))

;;
;; Elixir-mode
;;
;; ELPA package isn't good enough yet. Need to clone
;; https://github.com/elixir-lang/emacs-elixir into ~/.emacs.d and use that
;; instead.

(defun iex ()
  "Adds \"-e File.cd('MIXDIR') -S mix\" flags to iex when
  starting the iex comint buffer."
  (interactive)
  (let* ((mixfile (get-closest-pathname "mix.exs"))
         (dir (file-name-directory mixfile))
         (abs-dir (if (equal "~" (substring dir 0 1))
                      (concat (getenv "HOME") (substring dir 1))
                    dir)))
    ;; The rest of this does what elixir-mode-iex does, but it's set up to
    ;; take the raw prefix arg, not a string or list containing additional
    ;; args.
    (unless (comint-check-proc "*IEX*")
      (set-buffer
       (apply 'make-comint "IEX" elixir-iex-command nil
              (list "-e" (concat "File.cd('" abs-dir "')") "-S" "mix"))))
    (pop-to-buffer "*IEX*")))

(when (file-exists-p "~/.emacs.d/emacs-elixir/elixir-mode.el")
  (add-to-list 'load-path "~/.emacs.d/emacs-elixir")
  (require 'elixir-mode)
  (add-hook 'elixir-mode-hook
            '(lambda ()
               (define-key elixir-mode-map "\C-cd" 'debug-comment)
               (define-key elixir-mode-map "\r" 'newline-and-indent))))

;;
;; Lua-mode
;;
(autoload 'lua-mode "lua-mode" "Lua mode" t nil)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;;
;; ChucK-mode
;;
(autoload 'chuck-mode "chuck-mode" "ChucK mode" t nil)
(add-to-list 'auto-mode-alist '("\\.ck$" . chuck-mode))

;;
;; Scala-mode
;;
;; You might need to "sbaz install scala-tool-support" which puts emacs support
;; into /usr/local/scala/misc/scala-tool-support/emacs"

(condition-case ex
    (progn
      (autoload 'scala-mode "scala-mode2" "Scala mode" t nil)
      (add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
      (defun run-scala-buffer ()
        (interactive)
        (save-buffer)
        (compile (concat "scala " (buffer-file-name))))
      (load "inf-sbt")
      (add-hook 'scala-mode-hook
                '(lambda ()
                   (define-key scala-mode-map [f1] my-shell) ; I don't use Speedbar
                   (define-key scala-mode-map "\r" 'newline-and-indent)
                   (define-key scala-mode-map "\C-cr" 'run-scala-buffer)))
      ;; That bright red face for vars is too annoying
      (set-face-attribute 'scala-font-lock:var-face nil :bold nil :foreground "red3"))
  (error nil))

;;; Derived from path-to-java-package in progmodes/my-java-mode.el
(defun path-to-scala-package (path)
  "Returns a Scala package name for PATH, which is a file path.
Looks for 'src' or 'src/scala/{main,test}' in PATH and uses everything after
that, turning slashes into dots. For example, the path
/home/foo/project/src/main/scala/com/yoyodyne/project/Foo.scala becomes
'com.yoyodyne.project'. If PATH is a directory, the last part of
the path is ignored. That is a bug, but it's one I can live with
for now."
  (interactive)
  (let ((reverse-path-list (cdr (reverse (split-string path "/")))))
    (mapconcat
     'identity
     (reverse (upto reverse-path-list
		    (if (or (member "main" reverse-path-list)
                            (member "test" reverse-path-list))
                        "scala" "src")))
     ".")))

;;
;; Dired-mode
;;
(put 'dired-find-alternate-file 'disabled nil)
(defun my-dired-only-show (only-string)
  "Only show files that match a regular expression."
  (interactive "sOnly show files that match regexp: ")
  (dired-mark-files-regexp only-string)
  (dired-toggle-marks)
  (dired-do-kill-lines))

(defun my-dired-cruft-remove ()
  (interactive)
  (dired-mark-files-regexp
   "\\.\\(aux\\|class\\|o\\|elc\\|dvi\\|lj\\|log\\|toc\\)$")
  (dired-do-kill-lines))

(defun my-dired-dot-remove ()
  (interactive)
  (dired-mark-files-regexp "^\\.")
  (dired-do-kill-lines))

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "\C-c\C-c" 'my-dired-cruft-remove)
             (define-key dired-mode-map "\C-c." 'my-dired-dot-remove)
             (defvar dired-compress-file-suffixes
               '(("\\.gz\\'" "" "gunzip")
                 ("\\.tgz\\'" ".tar" "gunzip")
                 ("\\.Z\\'" "" "uncompress")
                 ;; For .z, try gunzip.  It might be an old gzip file,
                 ;; or it might be from compact? pack? (which?) but gunzip
                 ;; handles both.
                 ("\\.z\\'" "" "gunzip")
                 ;; This item controls naming for compression.
                 ("\\.tar\\'" ".tar.gz" nil))
  "Control changes in file name suffixes for compression and uncompression.
Each element specifies one transformation rule, and has the form:
  (REGEXP NEW-SUFFIX PROGRAM)
The rule applies when the old file name matches REGEXP.
The new file name is computed by deleting the part that matches REGEXP
 (as well as anything after that), then adding NEW-SUFFIX in its place.
If PROGRAM is non-nil, the rule is an uncompression rule,
and uncompression is done by running PROGRAM.
Otherwise, the rule is a compression rule, and compression is done with
gzip.")))

;;
;; Python-mode
;;
; Emacs now comes with its own Python mode called simply "python"
; (autoload 'python-mode "python-mode" "Python mode." t)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(defun run-python-buffer ()
  (interactive)
  (save-buffer)
  (compile (concat "python " (buffer-file-name))))
(add-hook 'python-mode-hook
          '(lambda ()
             (turn-on-font-lock)
             (define-key python-mode-map "\C-cr" 'run-python-buffer)
             ;; these two are in addition to the \C-< and \C-> bindings
             ;; that already exist in Pythong mode
             (define-key python-mode-map "\M-[" 'python-indent-shift-left)
             (define-key python-mode-map "\M-]" 'python-indent-shift-right)))

;;
;; SES-mode
;;
(autoload 'ses-mode "ses" "Spreadsheet mode" t)


;;
;; Set tab stops to eight chars, not four
;;
(defun eight-tab-stops ()
    (interactive)
      (setq tab-stop-list
            '(8 16 24 32 40 48 56 64 72 80))
        )

;;
;; Set tab stops to four chars, not eight
;;
(defun four-tab-stops ()
    (interactive)
      (setq tab-stop-list
            '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
        )

(defun tab-two () (interactive) (setq tab-width 2))
(defun tab-four () (interactive) (setq tab-width 4))
(defun tab-eight () (interactive) (setq tab-width 8))

(defun do-not-use ()
  (interactive)
  (error "Please don't use this"))

;;
;; aliases
;;
(defalias 'flfb 'font-lock-fontify-buffer)
(defalias 'run-hook 'run-hooks)
(defalias 't2 'tab-two)
(defalias 't4 'tab-four)
(defalias 't8 'tab-eight)

;;
;; Frame management
;;
(defun zoom-frame-width-cols ()
  (interactive)				; for testing
  (round (/ (float (display-pixel-width))
            (+ (float (frame-char-width)) 0.125))))

(defun zoom-frame-height-lines ()
  (interactive)				; for testing
  (- (round (/ (float (display-pixel-height))
               (float (frame-char-height))))
     4))                                ; not sure why we need this, actually

(defun zoom-frame ()
  (interactive)
  (set-frame-width
   nil
   (cond ((eq 80 (frame-width)) (zoom-frame-width-cols))
	 (t 80))))

(defun max-frame-height ()
  (interactive)
  (set-frame-height
   nil
   (zoom-frame-height-lines)))

; Time and time zone information, for calendar's sunrise-sunset and related
; funcs.
(setq calendar-load-hook (cons 'american-calendar calendar-load-hook))
(setq calendar-latitude 41.08)          ; + north, - south
(setq calendar-longitude -73.14)        ; + east, - west
(setq calendar-location-name "Fairfield, CT")

(setq calendar-time-zone -300)
(setq calendar-standard-time-zone-name "EST")
(setq calendar-daylight-time-zone-name "EDT")

; http://blog.plover.com/prog/revert-all.html
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer) 
                 (not (buffer-modified-p buffer)))
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshed open files"))

(defun cbo4 ()
  (interactive)
  (setq c-basic-offset 4)
  (setq ruby-indent-level 4))
(defun cbo2 ()
  (interactive)
  (setq c-basic-offset 2)
  (setq ruby-indent-level 2))

(defun address (str)
  "Find STR in my address book file. Looks first for STR as the
beginning of a name. If not found, looks for the first occurrence
of STR anywhere."
  (interactive "sSearch string: ")
  (let ((search-str (replace-regexp-in-string "+" " " str))
        (current-buf (current-buffer)))
    (find-file (concat *my-pim-dir* "orgs/address_book.org"))
    (goto-char (point-min))
    (or
     (search-forward (concat "\n\n" search-str) nil t)
     (search-forward (concat "\n" search-str) nil t)
     (search-forward search-str nil t)
     (error (concat "\"" search-str "\" not found")))))

(defun email (str)
  "Find first email of STR in my address book Org Mode file. For
some reason, the file's buffer remains in the front, even though
I'm using save-excursion."
  (interactive "sEmail address of: ")
  (save-excursion
    (address str)
    (search-forward "mailto:")
    (org-open-at-point)))

(defun open-email-client ()
  "Open an email client"
  (interactive)
  (shell-command "open mailto:"))

(defun word-count ()
  "Count words in buffer, reporting both (buffer size / 5) and wc -w"
  (interactive)
  (word-count-of-range (point-min) (point-max)))

(defun word-count-region ()
  "Count words in region, reporting both (buffer size / 5) and wc -w"
  (interactive)
  (word-count-of-range (region-beginning) (region-end)))

(defun word-count-of-range (beginning end)
  "Count words from beginning to end, reporting both (buffer size / 5)
and wc -w"
  (let* ((five-char-word-count (/ (- (point-max) (point-min)) 5))
	 (buffer-no-spaces
	  (replace-regexp-in-string
	   "[ \t\n\r]" ""
	   (buffer-substring-no-properties (point-min) (point-max))))
	 (five-char-no-whitespace-word-count
	  (/ (length buffer-no-spaces) 5)))
    (message (concat "five-char: "
		     (int-to-string five-char-word-count)
		     " / five-char-no-whitespace: "
		     (int-to-string five-char-no-whitespace-word-count)
		     " / words: "
		     (int-to-string (how-many "\\w+" beginning end))))))

;; From http://steve-yegge.blogspot.com/2007/02/my-save-excursion.html
(defun article-length ()
  "Print character and word stats on current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((char-count 0))
      (while (not (eobp))
        (unless (looking-at "[ \t\r\n]")
          (incf char-count))
        (forward-char 1))
      (message "%d chars, %d words" char-count (/ char-count 5)))))

;;; cfengine mode
(add-to-list 'auto-mode-alist '("\\.cf$" . cfengine-mode))

;;
;; Go mode
;;
(autoload 'go-mode "go-mode" t nil)
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
(add-hook 'go-mode-hook
          '(lambda ()
             (tab-four)
             (setq indent-tabs-mode t)))

;;
;; Haskell mode
;;
(autoload 'haskell-mode "haskell-mode" "Haskell mode" t nil)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;;
;; Hexl mode
;;
(defvar hexl-program (concat *my-emacs-lib-dir* "hexlify.rb"))

;;
;; http-twiddle
;;
(autoload 'http-twiddle-mode "http-twiddle" "HTTP twiddle mode" t nil)
(add-to-list 'auto-mode-alist '("\\.http-twiddle$" . http-twiddle-mode))

;;
;; C#
;;
(autoload 'csharp-mode "csharp-mode" "C# Mode" t nil)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;;
;; git
;;
(unless (boundp '*git-emacs-support-dir*)
  (defvar *git-emacs-support-dir* "/usr/local/src/git/contrib/emacs"))
(when (file-exists-p *git-emacs-support-dir*)
  (add-to-list 'load-path *git-emacs-support-dir* t)
  (autoload 'git-status "git" "git" t))
(setenv "GIT_PAGER" "cat")

;;
;; Growl (Mac OS X only)
;;
(defun growl-notify (message &optional title)
  "Display a Growl MESSAGE. The optional TITLE's default value is \"Emacs\"."
  (interactive "sMessage: ")
  (let ((g-title (if (and title (not (eq title ""))) title "Emacs")))
    (shell-command
     (concat
      "growlnotify"
      " --image /Applications/Emacs.app/Contents/Resources/Emacs.icns"
      " --title " (shell-quote-argument g-title)
      " --message " (shell-quote-argument message)))))

;;
;; rcirc
;;
(setq
 rcirc-default-server "irc.freenode.net"
 rcirc-default-nick "jmenard42"
; rcirc-authinfo '(("freenode" nickserv "jmenard42"))
; rcirc-startup-channels-alist
;   '(("\\.freenode\\.net$" "#emacs"))
)
  "A list of IRC channel names that are monitored for notifications."
(defvar my-rcirc-notifiy-channels
  '("#jimm-test"))
(defun my-rcirc-print-hook (process sender response target text)
  "Use Growl to tell me about IRC channel activity. Only notify
me about the channels listed in my-rcirc-notifiy-channels."
  (when (member target my-rcirc-notifiy-channels)
    (growl-notify (concat "<" sender ">"
                          (if (equal response "PRIVMSG") "" (concat " " response))
                          " " text)
                  target)))
              
(add-hook 'rcirc-print-hooks 'my-rcirc-print-hook)

;;
;; Org Mode
;;
(require 'org)
(unless (boundp 'org-ans1)
  (defvar org-ans1)
  (defvar org-ans2))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(defun my-org-execute-src ()
  (interactive)
  (let* ((info (org-edit-src-find-region-and-lang))
         (p-beg (car info))
         (p-end (cadr info))
         (lang (substring-no-properties (caddr info) 0))
         (tmpfile (make-temp-file "org-src-")))
          (write-region p-beg p-end tmpfile)
          (compile (concat lang " " tmpfile))))


;; recommended
(setq org-agenda-include-diary t)
(setq org-agenda-files (list (concat *my-pim-dir* "orgs/todo.org")))
(setq org-startup-folded 'content)
(add-hook 'org-mode-hook
          '(lambda ()
             (setq org-export-with-sub-superscripts nil)
             (define-key org-mode-map "\C-cr" 'my-org-execute-src)))

(set-face-attribute 'org-level-1 nil :height 1.2 :bold t)

; My own "addr:" link type
(defun my-org-addr-open (entry)
  "Find ENTRY in my address_book.org file."
  (address entry))
(when-fboundp-call org-add-link-type "addr" 'my-org-addr-open)

;;
;; HAML and SASS
;; Found {haml,sass}-mode.el files in the directory path-to-haml-gem/extra/.
;;
(autoload 'haml-mode "haml-mode" "haml mode")
(autoload 'sass-mode "sass-mode" "sass mode")
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.s\\(a\\|c\\)?ss$" . sass-mode))
(defun load-haml ()
  "A convenience function that loads Haml and Sass modes."
  (interactive)
  (load (concat *my-emacs-lib-dir* "progmodes/haml-mode"))
  (load (concat *my-emacs-lib-dir* "progmodes/sass-mode")))

;;
;; Org Present Mode
;;
;; https://github.com/rlister/org-present
(autoload 'org-present "org-present" nil t)
(defvar org-present-saved-cursor-color "black")

(add-hook 'org-present-mode-hook
          (lambda ()
            ;; (setq org-present-saved-cursor-color (frame-parameter nil 'cursor-color))
            ;; (set-cursor-color (if (equal "white" (frame-parameter nil 'background-color))
            ;;                       "ghost white"
            ;;                     "white"))
            (blink-cursor-mode 0)
            (org-present-big)
            (org-display-inline-images)))

(add-hook 'org-present-mode-quit-hook
          (lambda ()
            ;; (set-cursor-color org-present-saved-cursor-color)
            (blink-cursor-mode 1)
            (org-present-small)
            (org-remove-inline-images)))

;;
;; iTerm
;;
(defun tell-iterm (str)
  "Send str to the front window/session in iTerm."
  (interactive "siTerm input: ")
  (let ((str (replace-regexp-in-string "\"" "\\\"" str t t)))
    (do-applescript (concat
"tell application \"iTerm\"\n"
"	tell the current terminal\n"
"		tell the current session\n"
"			write text \"" str "\"\n"
"		end tell\n"
"	end tell\n"
"end tell\n"
))))
(defun send-current-line-to-iterm ()
  "Send the current line to iTerm using tell-iterm."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (push-mark)
    (end-of-line)
    (tell-iterm (buffer-substring-no-properties (point) (mark)))))

;;
;; Textile mode
;;
(autoload 'textile-mode "textile-mode" "textile mode")
(add-to-list 'auto-mode-alist '("\\.textile$" . textile-mode))
(add-hook 'textile-mode-hook
          '(lambda ()
             (auto-fill-mode 0)))

;;
;; LilyPond mode
;;
(defcustom LilyPond-ps-command "open"
  "Command used to display PS files."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-pdf-command "open"
  "Command used to display PDF files."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-midi-command "open"
  "Command used to play MIDI files."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-all-midi-command "open"
  "Command used to play MIDI files."
  :group 'LilyPond
  :type 'string)
(autoload 'LilyPond-mode "lilypond-init" "lilypond mode")
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook
          '(lambda ()
             (define-key LilyPond-mode-map "\C-c\C-k" 'compile)))

;;
;; KeyMaster
;;
(autoload 'keymaster-mode "keymaster-mode")
(add-to-list 'auto-mode-alist '("\\.km$" . keymaster-mode))

;;
;; Android
;;
(autoload 'android-mode "android-mode")

;;
;; Drools
;;
(autoload 'drools-mode "drools-mode" "Drools mode")
(add-to-list 'auto-mode-alist '("\\.drl$" . drools-mode))

;;
;; Smalltalk
;;
(add-to-list 'auto-mode-alist '("\\.st$" . smalltalk-mode))

;;
;; Roo command files
;;
(autoload 'roo-mode "roo-mode")
(add-to-list 'auto-mode-alist '("\\.roo$" . roo-mode))

;;
;; Uniquify
;; http://trey-jackson.blogspot.com/2008/01/emacs-tip-11-uniquify.html
;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;;
;; Encryption
;;
;; This is last because it modifies write-file-hooks. See the comment at the
;; beginning of crypt++.el.
;;
(require 'crypt++)
;; (setq crypt-encryption-type 'gpg
;;             crypt-encryption-file-extension "\\(Secure\\)$\\|\\(secure\\.org\\)$\\|\\(\\.enc\\)$")
;; (crypt-rebuild-tables)

(setq epa-file-cache-passphrase-for-symmetric-encryption t) ; remember passphrase for save

;;
;; Read RSS feeds via Gnus using Gwene (gwene.org)
;;
(setq gnus-site-init-file (concat *my-emacs-lib-dir* "gnus-init.el"))

;;
;; WebJump
;;
(setq webjump-sites
      '(
        ("gm (Google Mail)" .
         [simple-query "https://mail.google.com/mail/"
                       "https://mail.google.com/mail/#" ""])
        ("gr (Google Reader)" . "https://www.google.com/reader/view/?tab=my")
        ("gg (Google)" .
         [simple-query "http://www.google.com/"
                       "http://www.google.com/search?q=" ""])
        ("reddit" . "http://www.reddit.com/")
        ("preddit" . "http://www.reddit.com/r/programming/")
        ("hn" . "http://news.ycombinator.com/")
        ("map (Google Maps)" .
         [simple-query "http://maps.google.com/"
                       "http://maps.google.com/?q=" ""])
        ("gotapi" . "http://www.gotapi.com/html")
        ("wikip (Wikipedia)"
         [simple-query "http://wikipedia.org/"
                       "http://wikipedia.org/wiki/" ""])
        ("subreddit" .
         [simple-query "http://www.reddit.com/"
                       "http://www.reddit.com/r/" "/"])
        ("phpfunc (PHP function lookup)" .
         [simple-query "http://www.php.net/manual/en/"
                       "http://www.php.net/manual/en/function." ".php"])
        ))

;;
;; Dedicated window toggle
;; http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
;;
(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

;;
;; Time manipulation
;;
(defun add-times (&rest time-strings)
  "Takes a list of hour:minute time strings such as \"1:23\" or
\"0:44\" adds them up, and returns a string in the same format."
  (let ((parsed (mapcar 'parse-time-string time-strings)))
    (format-seconds 
     "%h:%02m"
     (+
      (apply '+ (mapcar (lambda (p) (* 60 (cadr p))) parsed)) ; minutes
      (apply '+ (mapcar (lambda (p) (* 3600 (caddr p))) parsed)))))) ; hours

;;
;; Scrambling a word
;;
(defun word-at-point ()
  (save-excursion
    (forward-word)
    (push-mark)
    (backward-word)
    (buffer-substring-no-properties (point) (mark))))

(defun do-scramble-word (word scrambled)
  (if (zerop (length word))
      scrambled
    (let ((i (random (length word))))
    (do-scramble-word
     (concat (substring word 0 i) (substring word (1+ i)))
     (concat scrambled (substring word i (1+ i)))))))

(defun scramble-word ()
  (interactive)
  (let ((word (do-scramble-word (word-at-point) "")))
    (save-excursion
      (forward-word)
      (backward-kill-word 1)
      (insert word))))

;;
;; Reorganize current frame's buffers to show current buffer and current
;; buffer's directory.
;;
(defun center-of-attention ()
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (dired "."))

;;
;; Reformat my bank's transactions CSV file
;;
(defun reformat-bank-transactions ()
  (interactive)

  (beginning-of-buffer)
  (search-forward "<Date>,<CheckNum>")
  (end-of-line)
  (forward-char)
  (delete-region 1 (point))

  (end-of-buffer)
  (search-backward "/")
  (end-of-line)
  (forward-char)
  (push-mark)
  (end-of-buffer)
  (delete-region (point) (mark))

  (beginning-of-buffer)
  (while (search-forward "" nil t)
    (replace-match "" nil t))
  (beginning-of-buffer)
  (while (search-forward " XXXXXX" nil t)
    (replace-match "" nil t))

  (mark-whole-buffer)
  (reverse-region (point) (mark)))

;;
;; Global key bindings
;;

(global-set-key "\M-z" 'zap-upto-char)
(global-set-key "\M-`" 'my-ff-find-other-file)
(global-set-key "\C-c1" 'find-grep-dired)
(global-set-key "\C-c2" 'grep-find)
(global-set-key "\C-c3" 'grep)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-x?" 'help-for-help)
(global-set-key "\C-x\C-k" 'compile)
(global-set-key "\C-x\C-m" 'open-email-client)
(global-set-key "\C-c\C-k" 'compile)
(global-set-key "\C-x\C-z" 'shrink-window)
(global-set-key "\M-\C-h" 'backward-kill-word)
(global-set-key "\M-\033" 'eval-expression)
(global-set-key "\M- " 'just-one-space)

(global-set-key [f1] my-shell)
(global-set-key [\C-f1] 'shell)
(global-set-key [f2] 'center-of-attention)
(global-set-key [\C-f2] 'remember)
(global-set-key [f3] 'magit-status)
(global-set-key [f4]
  '(lambda () (interactive) (find-file (concat *my-pim-dir* "orgs/todo.org"))))
(global-set-key [f5]
  '(lambda () (interactive) (switch-to-buffer "*inferior-lisp*")))
(global-set-key [\C-f5]
  '(lambda () (interactive) (switch-to-buffer "*SQL*")))
(global-set-key [f6]
  '(lambda ()
     (interactive)
     (find-file *my-remember-data-file*)
     (goto-char (point-max))))
(global-set-key [f7] 'my-url-open)
(global-set-key [\C-f7] 'my-javadoc-open)
(global-set-key [f8] 'ef)
(global-set-key [\C-f8]
  '(lambda (fname-regexp) (interactive "sOrg file regex: ")
     (ef (shell-quote-argument fname-regexp) (concat *my-pim-dir* "orgs/"))))
(global-set-key [f9] 'bookmark-jump)
(global-set-key [\C-f9] 'bookmark-set)
(global-set-key [f10] 'zoom-frame)
(global-set-key [\C-f10] 'max-frame-height)
(global-set-key [f11] 'other-window)

;; SMEX mode
(when (fboundp 'smex-initialize)
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x. (Was "C-c C-c M-x" in smex sample, but "C-c C-c" is
  ;; already taken by my comment-region binding.
  (global-set-key (kbd "C-c C-x M-x") 'execute-extended-command))

;; org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(global-set-key "\C-cw" 'toggle-current-window-dedication)
(set-register ?s "#+begin_src \n#+end_src")

;; Custom variable settings
(custom-set-variables
 '(abbrev-mode t)
 '(android-mode-sdk-dir "/usr/local/android-sdk-mac")
 '(css-indent-offset 2)
 '(eshell-save-history-on-exit t)
 '(sgml-xml-mode t)
 '(woman-use-own-frame nil)
 '(sgml-xml-mode t)
 '(safe-local-variable-values
   (quote ((org-publish-project-alist ("patchmaster"
                                       :base-directory "."
                                       :publishing-directory "../public_html"
                                       :style "<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"/>"
                                       :author "Jim Menard"
                                       :email "jim@jimmenard.com"))
           (org-publish-project-alist ("keymaster"
                                       :base-directory "."
                                       :publishing-directory "../public_html"
                                       :style "<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"/>"
                                       :author "Jim Menard"
                                       :email "jim@jimmenard.com"))
           (org-publish-project-alist ("trackmaster"
                                       :base-directory "."
                                       :publishing-directory "../public_html"
                                       :style "<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"/>"
                                       :author "Jim Menard"
                                       :email "jim@jimmenard.com"))
           (Syntax . Common-Lisp)))))

;; Custom faces
(custom-set-faces
 '(eshell-prompt
   ((((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "SteelBlue"))
    (t (:bold t)))))
