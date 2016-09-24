;;;; *my-emacs-lib-dir* must be defined. See "Bootstrap Process" in
;;;; README.org.

(defun bootstrap-file (domain machine file-name)
  "Returns the path to FILE-NAME for MACHINE in DOMAIN. The
variable *my-emacs-lib-dir* must be defined."
  (concat *my-emacs-lib-dir* "bootstrap/" domain "/" machine "/" file-name))

(defun load-init-if-exists (domain machine file)
  "Loads FILE for MACHINE in DOMAIN, if it exists.

Makes sure that an .elc file is loaded, but only if it exists and
is newer than the .el file."
  (let* ((f (bootstrap-file domain machine (concat file ".el")))
	 (f-elc (concat f "c")))
    (cond ((and (file-exists-p f-elc) (file-newer-than-file-p f-elc f))
	   (load-file f-elc))
	  ((file-exists-p f)
	   (load-file f)))))

(defun bootstrap-init (domain machine)
  "Defines a few global variables and settings in preparation for calling `bootstrap-load'.

These are two separate functions because `package-initialize'
INSISTS upon being in init.el now, so init.el keeps getting
rewritten if I don't have it in there. But I want to load it
before the bootstrap load files are actually loaded.

The variable *my-emacs-lib-dir* must be defined before calling
this function."
  (setq *my-emacs-bootstrap-domain* domain
        *my-emacs-bootstrap-machine* machine
        custom-file (bootstrap-file domain machine "custom.el"))
  (add-to-list 'load-path *my-emacs-lib-dir* t) ; add to end of load path
  (load custom-file))

(defun bootstrap-load ()
  "Finds the directory bootstrap/DOMAIN/MACHINE and loads three
files: \"before.el\" from that directory, \"emacs.el\" in this
directory, and \"after.el\" from that directory. Finally, it sets
the location of the bookmark save file to that directory.

The function `bootstrap-load' must be called before calling
this function."
  (load-init-if-exists *my-emacs-bootstrap-domain* *my-emacs-bootstrap-machine* "before")
  (load-library "emacs")
  (load-init-if-exists *my-emacs-bootstrap-domain* *my-emacs-bootstrap-machine* "after"))
