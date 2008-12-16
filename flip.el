;;
;; Flip back and forth between header and implementation files. This is a
;; *much* simplified version of ff-find-alternate-file.
;;
(defun flip ()
  (interactive)
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
	 (ff-entry (car ff-other-file-alist))
	 (ff-remainder (cdr ff-other-file-alist))
	 (found-p nil)
	 )
    ; Outer loop: loop over each entry in ff-other-file-alist and see if
    ; the car (the file-extension regexp) matches our file name.
    (while (and (not found-p) (not (null ff-entry)))
      (let* ((loc (string-match (car ff-entry) filename)))
	(if (not (null loc))
	    (let* ((base (substring filename 0 loc))
		   (new-extension-list (car (cdr ff-entry)))
		   (extension (car new-extension-list))
		   (new-extension-list (cdr new-extension-list)))
	      ; Inner loop: loop over each "other" file extension and look
	      ; for a readable file with that extension.
	      (while (and (not found-p) (not (null extension)))
		(let ((other-file-name (concat base extension)))
		  (if (file-readable-p other-file-name)
		      (progn
			(find-file other-file-name)
			(setq found-p t)
			)
		    )
		  (setq extension (car new-extension-list))
		  (setq new-extension-list (cdr new-extension-list))
		  )
		)
	      )
	  )
	(setq ff-entry (car ff-remainder))
	(setq ff-remainder (cdr ff-remainder))
	)
      )
    (if (not found-p)
	(error "can't find any matching other file"))
;;	(error (concat "can't find any other file that matches any of "
;;	(mapconcat 'symbol-name new-extension-list)))
    )
  )
