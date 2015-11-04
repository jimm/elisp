;;
;; Elixir-mode support. This is not elixir-mode itself, which is available
;; from ELPA/MELPA.
;;
;; Overlaps with Alchemist, which I've started using. I don't automatically
;; load this file any more.

(defvar iex-proc-name "IEX"
  "The name of the comint process for iex.")
(defun iex-buf-name ()
  "Returns the name of the iex buffer, derived from iex-proc-name."
  (concat "*" iex-proc-name "*"))

(defun iex--start (iex-opts)
  (let ((iex-cmd (or (and (boundp 'elixir-iex-command) elixir-iex-command)
                     "iex")))
    ;; The rest of this does what elixir-mode-iex does, but it's set up to
    ;; take the raw prefix arg, not a string or list containing additional
    ;; args.
    (unless (comint-check-proc (iex-buf-name))
      (set-buffer
       (apply 'make-comint iex-proc-name iex-cmd nil iex-opts)))
    (iex-switch-to-inf)))

(defun iex-mix ()
  "Adds \"-e File.cd('MIXDIR') -S mix\" flags to iex when
  starting the iex comint buffer."
  (interactive)
  (let* ((mixfile (get-closest-pathname "mix.exs"))
         (dir (file-name-directory mixfile))
         (abs-dir (if (equal "~" (substring dir 0 1))
                      (concat (getenv "HOME") (substring dir 1))
                    dir)))
    (iex--start (list "-e" (concat "File.cd('" abs-dir "')") "-S" "mix"))))

(defun iex ()
  "iex in a comint buffer"
  (interactive)
  (iex--start ()))

(defun iex-switch-to-inf ()
  "Switch to the iex buffer. Signals an error if it does not exist."
  (interactive)
  (if (get-buffer (iex-buf-name))
      (pop-to-buffer (iex-buf-name))
    (error "No current iex buffer.")))
