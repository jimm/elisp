;; -*- emacs-lisp -*-

;;; Note: this file should be loaded by bootstrap-init.el.

(require 'cl)

(defmacro when-fboundp-call (f &rest args)
  "If F is bound, calls it with ARGS."
  `(when (fboundp (function ,f)) (funcall (function ,f) ,@args)))

(defmacro unless-boundp-setq (var val)
  "If VAR is not bound, sets it to VAL."
  `(unless (boundp (quote ,var)) (setq ,var ,val)))

(defun blank-p (val)
  "VAL is blank if it is `nil', the empty list, or an empty or
whitespace-only string."
  (or (not val)                         ; nil or empty list
      (and (stringp val) (not (nil-blank-string val)))
      nil))

(defun val-or-default (val default)
  "If VAL is not `blank-p' return it, else return DEFAULT."
  (if (not (blank-p val)) val default))

(if (< emacs-major-version 24)
  (let ((f (expand-file-name "~/.emacs.d/elpa/package.el")))
    (when (file-exists-p f)
      (load f)))
  ;; else
  (require 'package))
(when (fboundp #'package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
  (setq package--init-file-ensured t)   ; avoid check for being in init.el
  (package-initialize))

(defvar my-shell #'eshell
  "The shell to use inside Emacs; examples include 'shell or 'eshell.")
(defvar my-alternate-shell #'shell
  "Alternate shell. Bound to alternate key.")

(when (equal default-directory "/")
  (setq default-directory "~/"))

;; Add *my-emacs-lib-dir* subdirs to the end of load-path, so if it's
;; pre-installed that version is used first.
;;
;; Note that *my-emacs-lib-dir* should already be on the load path. It gets
;; added by bootstrap-init.el.
(mapc (lambda (dir)
        (add-to-list 'load-path (concat *my-emacs-lib-dir* dir "/") t))
      '("progmodes" "ses"))

;; See https://github.com/jwiegley/use-package

(use-package 2048-game
  :init
  (add-hook '2048-mode-hook
            (lambda ()
              (define-key 2048-mode-map "j" '2048-down)
              (define-key 2048-mode-map "k" '2048-up)
              (define-key 2048-mode-map "h" '2048-left)
              (define-key 2048-mode-map "l" '2048-right))))

(use-package ace-window
  :ensure t)

(use-package ag
  :ensure t
  :config
  (setq ag-arguments (list "--smart-case" "--nocolor" "--nogroup")))

(use-package bind-key
  :ensure t)

(use-package clojure
  :mode "\\.\\(cljs\\|boot\\)$"
  :init
  (add-hook 'lisp-mode-hook
            (lambda ()
              (define-key lisp-mode-map "\r" #'newline-and-indent)
              (define-key lisp-mode-map "\C-cd" #'debug-comment)))
  :config
  (load "my-clojure-mode")
  (setq inferior-lisp-program "lein repl")
  (defun lein-repl ()
    (interactive)
    (setq inferior-lisp-program "lein repl")
    (inferior-lisp "lein repl"))

  (defun boot-repl ()
    (interactive)
    (setq inferior-lisp "boot repl")
    (inferior-lisp "boot repl")))

(use-package compile
  :init
  (setq compilation-error-regexp-alist
        (cons
         ;; Maven 2 error messages are of the form file:[line,column]
         '("^\\(/[^:]+\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 1 2 3)
         (cons
          ;; Scala error messages
          '("\\(\\([a-zA-Z0-9]*/\\)*\\([a-zA-Z0-9]*\\.scala\\)\\):\\([0-9]*\\).*" 1 2)
          compilation-error-regexp-alist))))

(use-package csharp-mode)

(use-package groovy-mode
  :init
  (add-hook 'groovy-mode-hook
            (lambda ()
              (setq groovy-basic-offset 4)
              (define-key groovy-mode-map "\r" #'newline-and-indent)
              (define-key groovy-mode-map "\C-cr" 'executable-interpret)
              (font-lock-mode 1))))

(use-package inf-groovy)

(use-package inf-groovy-keys
  :init
  (add-hook 'groovy-mode-hook
            (lambda ()
              (inf-groovy-keys))))

(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t))

(use-package inf-clojure)

(use-package inf-sbt)

(use-package coffee-mode
  :mode "\\.cjsx$" "\\.coffee$" "Cakefile"
  :init
  (defun compile-coffee-buffer ()
    (interactive)
    (shell-command (concat "coffee"
                           " -o " (shell-quote-argument (file-name-directory (buffer-file-name)))
                           " -c " (shell-quote-argument (buffer-file-name)))))
  (add-hook 'coffee-mode-hook
            (lambda ()
              (setq coffee-js-mode #'javascript-mode)
              (define-key coffee-mode-map "\C-cr" 'executable-interpret)
              (define-key coffee-mode-map "\C-ck" #'compile-coffee-buffer)
              (set (make-local-variable 'tab-width) 2))))

(use-package dash
  :ensure t
  :config
  (dash-enable-font-lock))

(use-package deft
  :config
  (setq deft-extensions '("org" "txt" "md" "markdown")
        deft-directory (concat *my-pim-dir* "orgs/")
        deft-recursive t
        deft-use-filename-as-title t))

(use-package diminish
  :ensure t)

(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g k" . dumb-jump-back)
         ("M-g q" . dumb-jump-quick-look)))

(use-package elixir-mode
  :ensure t
  :init
  (add-hook 'elixir-mode-hook
            (lambda ()
              (define-key elixir-mode-map "\C-cd" #'debug-comment)
              (define-key elixir-mode-map "\C-cr" 'executable-interpret)
              (when-fboundp-call alchemist-mode))))

(use-package alchemist
  :init
  (add-hook 'alchemist-mode-hook
            (lambda ()
              (let ((dir (file-name-as-directory (getenv "ELIXIR_HOME"))))
                (when (file-exists-p dir)
                  (setq alchemist-goto-elixir-source-dir dir)))
              (let ((dir (file-name-as-directory (getenv "ERLANG_HOME"))))
                (when (file-exists-p dir)
                  (setq alchemist-goto-erlang-source-dir dir)))
              (define-key alchemist-mode-map "\C-c\C-z"
                #'alchemist-iex-project-run))))

(use-package elm-mode)

(use-package elm-yasnippets)

(use-package emms
  :bind (([C-f7] . emms-previous)
         ([C-f8] . emms-pause)	    ; toggles between pause and resume
         ([C-f9] . emms-next))
  :init
  (setq emms-source-file-default-directory
        (concat (file-name-directory (getenv "dbox")) "Music/music/"))
  :config
  (defun emms-init ()
    (interactive)
    (emms-all)
    (emms-default-players)))

(use-package flx-ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package fzf)

(use-package gnus
  :init
  (setq gnus-site-init-file (concat *my-emacs-lib-dir* "gnus-init.el"))
  (if (zerop (shell-command "which gnutls-cli >/dev/null 2>&1"))
      (setq starttls-use-gnutls t
            starttls-gnutls-program "gnutls-cli"
            starttls-extra-arguments nil)))

(use-package go-mode
  :init
  (add-hook 'go-mode-hook
            (lambda ()
              (tab-four)
              (setq indent-tabs-mode t))))

(use-package haml-mode
  :ensure t)

(use-package hamlet-mode)

(use-package haskell-mode
  :mode "\\.hs$")

(use-package http-twiddle
  :mode "\\.http-twiddle$")

(use-package java-mode
  :mode "\\.aj$" "\\.jsp$" "\\\.w[as]r$"
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (c-set-style "java")
              (if window-system (font-lock-mode 1))))
  :config
  (load "my-java-mode"))

(use-package js-mode
  :mode "\\.[agj]s$" "\\.jsx$"
  :init
  (setq js-indent-level 2               ; need both?????
        javascript-indent-level 2
        js2-basic-offset 2))

(use-package keymaster-mode
  :mode "\\.km$")

(use-package less-css-mode)

(use-package lua-mode)

(use-package magit
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode "\\.\\(md\\|markdown\\|mdown\\)$"
  :init
  (add-hook 'markdown-mode-hook
            (lambda ()
              (set-face-attribute 'markdown-header-delimiter-face nil :foreground "black")
              (set-face-attribute 'markdown-header-face-1 nil :foreground "blue" :height 1.2 :bold t)
              (set-face-attribute 'markdown-header-face-2 nil :foreground "brown")
              (set-face-attribute 'markdown-header-face-3 nil :foreground "darkgreen")
              (set-face-attribute 'markdown-header-face-4 nil :foreground "black")
              (set-face-attribute 'markdown-header-face-5 nil :foreground "black")
              (set-face-attribute 'markdown-header-face-6 nil :foreground "black"))))

(use-package objc-mode
  :init
  (add-hook 'objc-mode-hook
            (lambda ()
              (if window-system (font-lock-mode 1))
              (setq c-basic-offset 4))))

(use-package org
  :ensure t
  :config
  (unless (boundp 'org-ans1)
    (defvar org-ans1)
    (defvar org-ans2)))

(use-package org-present
  :init
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (hide-cursor)
              (org-display-inline-images)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (show-cursor)
              (org-remove-inline-images))))

(use-package perl-mode
  :init
  :bind (:map perl-mode-map
         ("\r" . #'newline-and-indent)
         ("\M-\C-h" . #'backward-kill-word)
         ("\C-cd" . #'debug-comment))
  (add-hook 'perl-mode-hook
            (lambda ()
              (setq c-basic-offset 2
                    c-tab-always-indent nil))))
(use-package ponylang-mode)

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-enable-caching t
        ;; "Projectile[%s]" is too long but `diminish' makes it disappear
        projectile-mode-line
        '(:eval (if (file-remote-p default-directory)
                    " prj"
                  (format " prj[%s]" (projectile-project-name))))))

(use-package projectile-rails)

(use-package python-mode
  :init
  (lambda () (turn-on-font-lock))
  :config
  ;; Can't use :bind because the functions aren't defined yet
  (add-hook 'python-mode-hook
            (lambda ()
              (define-key python-mode-map "\C-cr" #'executable-interpret)
              ;; these two are in addition to the \C-< and \C-> bindings
              ;; that already exist in Python mode
              (define-key python-mode-map "\M-[" #'python-indent-shift-left)
              (define-key python-mode-map "\M-]" #'python-indent-shift-right))))

(use-package remember
  :init
  (setq *my-remember-data-file* (concat *my-pim-dir* "orgs/notes.org"))
  (add-hook 'remember-mode-hook
            (lambda ()
              (setq remember-data-file *my-remember-data-file*
                    remember-diary-file diary-file))))

(use-package rspec-mode)

(use-package ruby-mode
  :ensure t
  :mode 
  "\\.r\\(b\\(w\\|x\\)?\\|html?\\|js\\)$"
  "\\([Rr]ake\\|[Cc]ap\\|[Gg]em\\)file$"
  "\\.rake$"
  "\\.gem\\(spec\\)?$"
  "\\.duby$"
  :config
  (load "my-ruby-mode"))

(use-package inf-ruby
  :config
  (load "my-ruby-mode"))

(use-package rust-mode)

(use-package sass-mode
  :mode "\\.s\\(a\\|c\\)?ss$")

(use-package scala-mode
  :init
  (add-hook 'scala-mode-hook
            (lambda ()
              (define-key scala-mode-map [f1] my-shell) ; I don't use Speedbar
              (define-key scala-mode-map "\r" #'newline-and-indent)
              (define-key scala-mode-map "\C-cr" 'executable-interpret)))
  :config
  ;; That bright red face for vars is too annoying
  (set-face-attribute 'scala-font-lock:var-face nil :bold nil :foreground "red3")
  ;; Derived from path-to-java-package in progmodes/my-java-mode.el
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
       #'identity
       (reverse (upto reverse-path-list
                      (if (or (member "main" reverse-path-list)
                              (member "test" reverse-path-list))
                          "scala" "src")))
       "."))))
  

(use-package scheme-mode
  :init
  (add-hook 'scheme-mode-hook
            (lambda ()
              (define-key scheme-mode-map "\r" #'newline-and-indent)
              (define-key scheme-mode-map "\C-cd" #'debug-comment))))

(use-package ses-mode)

(use-package shenzhen-io-mode
  :mode "\\.szio$")

(use-package sicp)

(use-package smalltalk-mode)

(use-package smex
  :ensure t)

(use-package sql-mode
  :mode "\\.mysql$"
  :config
  (load "my-sql-mode"))

;; My own tools for keeping a daily status file up to date.
(use-package status)

(use-package textile-mode
  :ensure t
  :init
  (add-hook 'textile-mode-hook
            (lambda ()
              (auto-fill-mode 0)
              (visual-line-mode 1)
              (set-face-attribute 'textile-h1-face nil :foreground "blue" :height 1.2 :bold t)
              (set-face-attribute 'textile-h2-face nil :foreground "brown" :height 1.0)
              (set-face-attribute 'textile-h3-face nil :foreground "darkgreen" :height 1.0)
              (set-face-attribute 'textile-h4-face nil :foreground "black" :height 1.0)
              (set-face-attribute 'textile-h5-face nil :foreground "black" :height 1.0)
              (set-face-attribute 'textile-h6-face nil :foreground "black" :height 1.0))))

(use-package toml-mode)

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t  ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yaml-mode
  :mode "\\.ya?ml$")

(load "my-skeletons")

(require 'generic-x); DOS batch, ini files and much more
(add-to-list 'auto-mode-alist
             '("\\.properties$" . java-properties-generic-mode))

;;
;; Global variable settings and options
;;
(when-fboundp-call appt-activate 1)	; appointment notification

;; Ubuntu stuff
;(menu-bar-enable-clipboard)
(setq x-select-enable-clipboard t
      mouse-drag-copy-region t
      skeleton-pair nil)
(when-fboundp-call set-scroll-bar-mode 'right)
(mouse-wheel-mode 1)

(when (>= emacs-major-version 23)
  ;; Time to try transient mark mode for a while
  ;; (transient-mark-mode -1)
  (setq confirm-nonexistent-file-or-buffer nil))

(turn-on-auto-fill)
(auto-fill-mode 1)
(show-paren-mode 1)

(setq save-flag 1                       ; see bootstrap-ini for loc of file
      sentence-end-double-space nil
      column-number-mode nil
      make-backup-files nil             ; don't make backup files
      delete-auto-save-files t          ; no "#" files after a save
      auto-save-list-file-prefix nil    ; don't record sessions
      inhibit-startup-screen t          ; kill the startup message
      initial-scratch-message nil       ; used by Emacs 23 and above
      inhibit-startup-echo-area-message "jimm"
      Man-notify 'aggressive            ; when man found, jump there *immed*
      dabbrev-case-replace nil          ; preserve case when expanding
      mode-require-final-newline nil    ; do not force newlines
      ns-pop-up-frames nil              ; do not create new frames on Mac
      auto-revert-verbose nil        ; no message on each auto-revert update
      isearch-lax-whitespace nil
      visible-bell t
      version-control 'never            ; when to make backup files
      vc-handled-backends '()           ; disable VC minor mode
      frame-title-format '((:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name))
                                    "%b")))
      eww-search-prefix "https://www.google.com.com/?q=")

(setq-default fill-column 76
              indent-tabs-mode nil)


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(fset #'yes-or-no-p #'y-or-n-p)        ; accept simple 'y'/space, 'n'/delete
(unless (fboundp #'string-match-p) (defalias #'string-match-p #'string-match))

(defun display-startup-echo-area-message ()
  (message ""))

(when-fboundp-call tool-bar-mode -1)
(when-fboundp-call tooltip-mode -1)
(when-fboundp-call menu-bar-mode nil)

(global-font-lock-mode t)               ; always turn on, where available

(defun unix-file ()
  "Change the current buffer to UTF-8 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t))
(defun dos-file ()
  "Change the current buffer to UTF-8 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos t))

(defun underscores-to-camel-case (str)
  "Converts STR, which is a word using underscores, to camel case."
  (interactive "S")
  (apply #'concat (mapcar #'capitalize (split-string str "_"))))

;; I prefer zap-upto-char most of the time
(defun zap-upto-char (arg char)
  "Kill up to but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (read-char "Zap up to char: " t)))
  (zap-to-char arg char)
  (insert-char char)
  (backward-char))

(defun generate-random-password (password-length)
  "Generate a random password PASSWORD-LENGTH characters long.
Characters are selected from upper- and lower-case letters,
numbers, and punctuation."
  (let* ((chars "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!?@#$%^&*-_=+/.,")
         (chars-len (length chars)))
    (mapconcat (lambda (dummy)
                 (let ((idx (random chars-len)))
                   (substring chars idx (+ idx 1))))
               (number-sequence 0 (- password-length 1))
               "")))

(defun insert-random-password (arg)
  "Generate a random password ARG characters long (16 by default) and
insert it at point. See `generate-random-password`."
  (interactive "p")
  (insert (generate-random-password arg)))

;;; Avoid ringing the bell when caused by certain commands.
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer
                        mwheel-scroll down up next-line previous-line
                        backward-char forward-char))
          (ding))))

;;
;; Colorization
;;

(defun hello-darkness-my-old-friend ()
  (interactive)
  (ignore-errors
    (set-foreground-color "white")
    (set-background-color "grey20")
    (set-face-attribute 'mode-line nil :foreground "black" :background "grey75")))

(defun lighten-up ()
  (interactive)
  (ignore-errors
    (set-foreground-color "black")
    (set-background-color "GhostWhite")
    (set-face-attribute 'mode-line nil :foreground "yellow" :background "black")))

;;;
;;; Project-level navigation and search.
;;;
;;; Must be loaded after things like fzf are loaded.
;;;
(load "proj")

;;
;; For flipping back and forth between files and headers, or Java/Ruby files
;; and Java/Ruby test files. See also rails-find-other-file.
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
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

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

;;
;; Browse away!
;;

(defun my-goto-calendar-date (str)
  (interactive "sDate: ")
  (let ((date-list (cdddr (parse-time-string str))))
    (calendar)
    (calendar-goto-date (list (cadr date-list)
			      (car date-list)
			      (caddr date-list)))))

;(setq browse-url-generic-program "mozilla-firefox")
(setq browse-url-generic-program "open")

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

;;
;; Tramp
;;
; (setq tramp-default-method "scp")
(defun debug-tramp ()
  (interactive)
  (setq tramp-debug-buffer t
        tramp-verbose 10))

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
          (lambda ()
            (auto-fill-mode 1)
            (four-tab-stops)))

;;
;; For both C and C++ mode
;;
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 2
                  c-tab-always-indent nil
                  c-recognize-knr-p nil)

            ;; (define-key c-mode-map "{" #'skeleton-pair-insert-maybe)
            ;; (define-key c-mode-map "(" #'skeleton-pair-insert-maybe)

            ;; (local-set-key "\M-o" #'fh-open-header-file-other-window)
            ;; (local-set-key "\M-O" #'fh-open-header-file-other-frame)
            (local-set-key "\r" #'newline-and-indent)
            (autoload #'fh-open-header-file-other-window "find-header"
              "Locate header file and load it into other window" t)
            (autoload #'fh-open-header-file-other-frame "find-header"
              "Locate header file and load it into other frame" t)))

(defun cbo4 ()
  (interactive)
  (setq c-basic-offset 4
        ruby-indent-level 4))
(defun cbo2 ()
  (interactive)
  (setq c-basic-offset 2
        ruby-indent-level 2))

;;
;; C++-mode
;;
(add-to-list 'auto-mode-alist '("\\.[ch]pp?$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h?$" . c++-mode)) ; mostly C++, now a days
(add-to-list 'auto-mode-alist '("\\.pde$" . c++-mode)) ; Arduino
(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "stroustrup")
            (setq c-basic-offset 2)))

;;
;; Erlang-mode
;;
(when (and (boundp '*my-erlang-emacs-tools-dir*)
	   (file-exists-p *my-erlang-emacs-tools-dir*))
  (add-to-list 'load-path *my-erlang-emacs-tools-dir* t))
(autoload #'erlang-mode "erlang" "Erlang mode" t nil)
(add-to-list 'auto-mode-alist '("\\.[he]rl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.yaws$" . erlang-mode))
(add-hook 'erlang-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (comment-set-column 32)))

;;
;; Environment variables and path. Use "launchctl setenv var value" to set
;; environment vars on OS X so that they get passed to GUI apps like
;; Emacs.app when they are launched. Since Mac OS X is pretty much all I use
;; these days, I've put this code here. Shouldn't do any harm if run on
;; another flavor of Unix.
;;

;; For each PATH element, prepend it to exec-path if it's not already there.
(mapc (lambda (path)
        (add-to-list 'exec-path path))
      (split-string (getenv "PATH") ":"))

;;
;; sh-mode
;;
(add-hook 'sh-mode-hook
          (lambda ()
            (define-key sh-mode-map "\C-c\C-k" #'compile)
            (define-key shell-mode-map "\C-c\C-c" #'comment-region)))

;;
;; Eshell-mode
;; must come after defining ef
;;
(when (or (eq my-shell #'eshell)
          (eq my-alternate-shell #'eshell))
  (load "eshell")
  (load "eshell-customize"))

;;
;; Shell-mode
;;

;; Don't echo passwords
(add-hook 'comint-output-filter-functions
          #'comint-watch-for-password-prompt)
(setq shell-completion-execonly nil)    ; Any file is completion candidate
(add-hook 'shell-mode-hook
          (lambda ()
            (auto-fill-mode -1)
            (setq comint-scroll-show-maximum-output nil)))

;;
;; LaTeX-mode
;;
(eval-after-load "latex-mode"
  (progn
    (setq tex-dvi-view-command "latex-view '*'"  ; in my ~/bin dir
          ;; tex-dvi-view-command "xdvi -expert -hush"
          tex-dvi-print-command "dvips -f * | lpr")

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
              (lambda () (define-key tex-mode-map "\C-c\C-k" #'compile)))
    (add-hook 'latex-mode-hook
              (lambda ()
                (define-key latex-mode-map "\C-c\C-p" #'tex-print)
                ;; (define-key latex-mode-map "\C-c\C-t" #'my-tex-to-text)
                (define-key latex-mode-map "\C-c\C-k" #'compile)
                (define-key latex-mode-map "\C-c\C-i" #'find-mine)
                (define-key latex-mode-map "\C-c\C-s" #'my-tex-slide-dvi-view)))))

;;
;; Clisp and SBCL
;;

;; (require 'slime)
;; (slime-setup)

;; Clisp
(defun clisp ()
  (interactive)
  (setq inferior-lisp-program "clisp")
  (inferior-lisp "clisp"))

;; SBCL
(defun sbcl ()
  (interactive)
  (setq inferior-lisp-program "sbcl")
  (inferior-lisp "sbcl"))

;;
;; Emacs-Lisp-mode
;;
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map "\C-cd" #'debug-comment)
            (define-key emacs-lisp-mode-map "\r" #'newline-and-indent)))

;;
;; HTML-mode and SGML-mode
;;
(eval-after-load "html-mode"
  (progn
    (defun my-html-insert-comment ()
      (interactive "*")
      (insert "<!--  -->")
      (backward-char 4))

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
                       (cons (concat "\\." ext "$") #'sgml-mode)))
                                        ; jwcs, application, page: Tapestry
                                        ; ftl: FreeMarker
        '("xsd" "wsd[ld]" "jwcs" "application" "page" "ftl"))

(eval-after-load "sgml-mode"
  (progn
    (add-hook 'sgml-mode-hook
              (lambda ()
                (require 'tex-mode)
                (auto-fill-mode 1)
                (define-key sgml-mode-map "\C-c\C-k" #'compile)))

    (add-hook 'html-mode-hook
              (lambda ()
                (auto-fill-mode 1)
                (define-key html-mode-map "\C-c;" #'my-html-insert-comment)
                (define-key html-mode-map "\C-cp" #'php-mode)
                ;; The remaining functions are defined in my-ruby-mode.el
                (define-key html-mode-map "\C-ce" #'erb-eval-skeleton)
                (define-key html-mode-map "\C-cp" #'erb-print-skeleton)
                (define-key html-mode-map "\C-ch" #'insert-ruby-hash-arrow)
                (define-key html-mode-map "\C-cl" #'rails-link-to-skeleton)
                (define-key html-mode-map "\C-cr" #'rails-render-partial-skeleton)))))

;;
;; CSS-mode
;;
(autoload #'css-mode "css-mode" "CSS mode" t nil)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

;;
;; Crystal-mode
;;
(add-to-list 'load-path (concat user-emacs-directory "emacs-crystal-mode/") t)

(autoload #'crystal-mode "crystal-mode" "Crystal mode" t nil)
(add-to-list 'auto-mode-alist '("\\.cr$" . crystal-mode)) ; Crystal
(add-hook 'crystal-mode-hook
          (lambda ()
            (define-key crystal-mode-map "\C-cr" #'executable-interpret)))

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
          (lambda ()
            (define-key dired-mode-map "\C-c\C-c" #'my-dired-cruft-remove)
            (define-key dired-mode-map "\C-c." #'my-dired-dot-remove)
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
;; Dealing with ANSII color codes
;;
(defun remove-colorization ()
  "Remove ANSI color codes from the current buffer."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (replace-regexp "\\[[0-9]+m" "" nil (point-min) (point-max)))))

;; http://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
(require 'ansi-color)
(defun colorize-current-buffer ()
  "Display ANSI color codes correctly in the *compilation* buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-current-buffer)

;;
;; Set tab stops to eight chars, not four
;;
(defun eight-tab-stops ()
  (interactive)
  (setq tab-stop-list
        '(8 16 24 32 40 48 56 64 72 80)))

;;
;; Set tab stops to four chars, not eight
;;
(defun four-tab-stops ()
  (interactive)
  (setq tab-stop-list
        '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)))

(defun tab-two () (interactive) (setq tab-width 2))
(defun tab-four () (interactive) (setq tab-width 4))
(defun tab-eight () (interactive) (setq tab-width 8))

;;
;; aliases
;;
(defalias #'flfb #'font-lock-fontify-buffer)
(defalias #'run-hook #'run-hooks)
(defalias #'t2 #'tab-two)
(defalias #'t4 #'tab-four)
(defalias #'t8 #'tab-eight)

;;
;; Frame management
;;
(defcustom *zoom-frame-width-factor* 0.125
  "Fudge factor for display column width calculations.")
(defcustom *zoom-frame-height-diff* 4
  "Fudge factor for display column height calculations.")

(defvar *zoom-frame-saved-width-cols* nil)

(defun zoom-frame-width-cols ()
  (interactive)				; for testing
  (round (/ (float (display-pixel-width))
            (+ (float (frame-char-width)) *zoom-frame-width-factor*))))

(defun zoom-frame-height-lines ()
  (interactive)				; for testing
  (- (round (/ (float (display-pixel-height))
               (float (frame-char-height))))
     *zoom-frame-height-diff*))

(defun zoom-frame ()
  (interactive)
  (let ((frame (selected-frame)))
    ;; (set-frame-position frame 0 0)
    (set-frame-width
     frame
     (cond ((eq 80 (frame-width)) (or *zoom-frame-saved-width-cols* (zoom-frame-width-cols)))
           (t (progn
                (setq *zoom-frame-saved-width-cols* (frame-width))
                80))))))

(defun max-frame-height ()
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-position frame 0 0)
    (set-frame-height frame (zoom-frame-height-lines))))

;; Time and time zone information, for calendar's sunrise-sunset and related
;; funcs.
(when-fboundp-call calendar-set-date-style 'american)
(setq
 calendar-latitude 41.08                ; + north, - south
 calendar-longitude -73.14              ; + east, - west
 calendar-location-name "Fairfield, CT"
 calendar-time-zone -300
 calendar-standard-time-zone-name "EST"
 calendar-daylight-time-zone-name "EDT")

;; http://blog.plover.com/prog/revert-all.html
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
      (setq list (cdr list)
            buffer (car list))))
  (message "Refreshed open files"))

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

;;
;; Hexl mode
;;
(defvar hexl-program (concat *my-emacs-lib-dir* "hexlify.rb"))

;;
;; rcirc
;;
(setq
 rcirc-default-server "irc.freenode.net"
 rcirc-default-nick "jmenard42"
 ;; rcirc-authinfo '(("freenode" nickserv "jmenard42"))
 ;; rcirc-startup-channels-alist
 ;;   '(("\\.freenode\\.net$" "#emacs"))
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

(add-hook 'rcirc-print-hooks #'my-rcirc-print-hook)


;;
;; Org Mode extras
;;

(defun my-org-execute-src ()
  "Saves current Org mode src block to a temp file and executes
it in a compilation buffer by using the source language
name (e.g., \"sh\", \"ruby\") as a command. Obviously doesn't
work for all langauges."
  (interactive)
  (let* ((props (cadr (org-element-context)))
         (p-beg (- (plist-get props :begin) 1))
         (p-end (- (plist-get props :end) 1))
         (lang (plist-get props :language))
         (tmpfile (make-temp-file "org-src-")))
    (write-region p-beg p-end tmpfile)
    (compile (concat lang " " tmpfile))))

(defun lower-case-org-mode-templates ()
  "I like lower-case Org Mode templates. This function returns a
copy of org-structure-template-alist with lower-case template
values."
  (mapcar (lambda (entry)
            (list (car entry)
                  (downcase (cadr entry))
                  (caddr entry)))
          org-structure-template-alist))

;; The first three are recommended
(setq org-agenda-include-diary t
      org-agenda-files (list (concat *my-pim-dir* "orgs/todo.org"))
      org-startup-folded 'nofold
      org-src-fontify-natively t)

(add-hook 'org-mode-hook
          (lambda ()
            (org-add-link-type "addr" #'address)
            (org-add-link-type "date" #'my-goto-calendar-date)
            (setq org-export-with-sub-superscripts nil
                  org-structure-template-alist (lower-case-org-mode-templates))
            (define-key org-mode-map "\C-cr" #'my-org-execute-src)
            (define-key org-mode-map "\C-ct" #'org-toggle-link-display)
            ;; yasnippet mode
            ;; TODO org-set-local has gone away. Delete this call when all
            ;; of my Emacs instances are updated
            (when-fboundp-call org-set-local 'yas-trigger-key "\t")
            (add-to-list 'org-tab-first-hook
                         (lambda ()
                           (let ((yas/fallback-behavior 'return-nil))
                             (yas/expand))))
            (define-key yas-keymap "\t" 'yas-next-field-or-maybe-expand)))

(set-face-attribute 'org-level-1 nil :height 1.2 :bold t)
(set-face-attribute 'org-level-2 nil :foreground "black" :bold t)
(when (>= emacs-major-version 24)
  (set-face-attribute 'org-block nil :foreground "black"))
(setq org-fontify-whole-heading-line t) ; bg color covers whole line

;;
;; Cursor manipulation
;;

(defun show-cursor ()
  (interactive)
  (internal-show-cursor nil t)
  (blink-cursor-mode 10))

(defun hide-cursor ()
  (interactive)
  (blink-cursor-mode -1)
  (internal-show-cursor nil nil))

;;
;; Sending text to iTerm and similar functions
;;
(load "iterm")

(defun line-to-other-window ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (push-mark)
    (end-of-line)
    (kill-ring-save (point) (mark))

    (other-window 1)
    (end-of-buffer)
    (yank)
    (comint-send-input)

    (other-window -1))

  (end-of-line)
  (forward-char))

(defun region-to-other-window ()
  (interactive)
  (save-excursion
    (let ((str (buffer-substring-no-properties (point) (mark))))
      (other-window 1)
      (end-of-buffer)
      (insert str)
      (comint-send-input)
      (other-window -1))))

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
(autoload #'LilyPond-mode "lilypond-init" "lilyPond mode")
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook
          (lambda ()
            (define-key LilyPond-mode-map "\C-c\C-k" #'compile)))

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
  (let ((parsed (mapcar #'parse-time-string time-strings)))
    (format-seconds
     "%h:%02m"
     (+
      (apply #'+ (mapcar (lambda (p) (* 60 (cadr p))) parsed)) ; minutes
      (apply #'+ (mapcar (lambda (p) (* 3600 (caddr p))) parsed)))))) ; hours

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

;;; Key bindings, both common and local to the current machine.
;;; See README.org.
(load "keys")
