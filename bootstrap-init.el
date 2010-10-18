;;; *my-emacs-lib-dir* must be defined

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
  (setq *my-emacs-bootstrap-domain* domain)
  (setq *my-emacs-bootstrap-machine* machine)
  (add-to-list 'load-path *my-emacs-lib-dir* t) ; add to end of load path
  (load-init-if-exists domain machine "before")
  (load-library "emacs")
  (load-init-if-exists domain machine "after")
  (setq bookmark-default-file
	(bootstrap-file domain machine "emacs.bmk")))
