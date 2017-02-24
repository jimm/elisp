(defvar *my-pim-dir* "~/pim/")

;;; A really, really dumb version of use-package. Ensure isn't working yet.
(defmacro use-package (sym &rest args) 
   (let ((init (cadr (member :init args)))
	 (config (cadr (member :config args)))
         (ensure (cadr (member :ensure args))))
     `(ignore-errors
       (when ,ensure
         (package-install (symbol-name ,sym)))
       (require (quote ,sym))
       (progn ,init)
       (progn ,config))))

(load-file (concat user-emacs-directory "package.el"))
(package-initialize)
