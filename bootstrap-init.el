;;;; See "Bootstrap Process" in README.org.

(defvar *my-emacs-lib-dir* (file-name-directory load-file-name))

(defun bootstrap-file (file-name)
  "Returns the path to FILE-NAME in `*my-emacs-lib-dir*' for
`*my-emacs-bootstrap-machine*' in `*my-emacs-bootstrap-domain*'."
  (concat *my-emacs-lib-dir* "bootstrap/" *my-emacs-bootstrap-domain* "/"
          *my-emacs-bootstrap-machine* "/" file-name))

(defun load-bootstrap-file-if-exists (file)
  "Loads FILE for `*my-emacs-bootstrap-machine*' in
`*my-emacs-bootstrap-domain*', if it exists.

Makes sure that an .elc file is loaded, but only if it exists and
is newer than the .el file."
  (let* ((f (bootstrap-file (concat file ".el")))
	 (f-elc (concat f "c")))
    (cond ((and (file-exists-p f-elc) (file-newer-than-file-p f-elc f))
	   (load-file f-elc))
	  ((file-exists-p f)
	   (load-file f)))))

(defun bootstrap-init (domain machine)
  "Finds the directory bootstrap/DOMAIN/MACHINE and loads three
files: \"before.el\" from that directory, \"emacs.el\" in this
directory, and \"after.el\" from that directory. Finally, it sets
the location of the bookmark save file to that directory.

The variable `*my-emacs-lib-dir*' must be defined before calling
this function."
  (setq
   *my-emacs-bootstrap-domain* domain
   *my-emacs-bootstrap-machine* machine
   custom-file (bootstrap-file "custom.el")
   package--init-file-ensured t)    ; avoid check for being in init.el
  (add-to-list 'load-path *my-emacs-lib-dir* t) ; add to end of load path
  (load-file custom-file)
  (when (eq system-type 'darwin)
    (load-file (concat *my-emacs-lib-dir* "bootstrap/mac-before.el")))
  (load-bootstrap-file-if-exists "before")
  (load-library "emacs")
  (when (eq system-type 'darwin)
    (load-file (concat *my-emacs-lib-dir* "bootstrap/mac-after.el")))
  (load-bootstrap-file-if-exists "after"))
