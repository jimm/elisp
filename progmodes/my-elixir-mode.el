;;
;; Elixir-mode
;;
;; ELPA package isn't good enough yet. Need to clone
;; https://github.com/elixir-lang/emacs-elixir into ~/.emacs.d and use that
;; instead.

(defvar iex-proc-name "IEX"
  "The name of the comint process for iex.")
(defun iex-buf-name ()
  "Returns the name of the iex buffer, derived from iex-proc-name."
  (concat "*" iex-proc-name "*"))

(defun iex-mix ()
  "Adds \"-e File.cd('MIXDIR') -S mix\" flags to iex when
  starting the iex comint buffer."
  (interactive)
  (let* ((mixfile (get-closest-pathname "mix.exs"))
         (dir (file-name-directory mixfile))
         (abs-dir (if (equal "~" (substring dir 0 1))
                      (concat (getenv "HOME") (substring dir 1))
                    dir))
         (iex-opts (list "-e" (concat "File.cd('" abs-dir "')") "-S" "mix")))
    ;; The rest of this does what elixir-mode-iex does, but it's set up to
    ;; take the raw prefix arg, not a string or list containing additional
    ;; args.
    (unless (comint-check-proc (iex-buf-name))
      (set-buffer
       (apply 'make-comint iex-proc-name (or elixir-iex-command "iex") nil iex-opts)))
    (iex-switch-to-inf)))

(defun iex ()
  "iex in a comint buffer"
  (interactive)
  (let* ((mixfile (get-closest-pathname "mix.exs"))
         (dir (file-name-directory mixfile))
         (abs-dir (if (equal "~" (substring dir 0 1))
                      (concat (getenv "HOME") (substring dir 1))
                    dir)))
    ;; The rest of this does what elixir-mode-iex does, but it's set up to
    ;; take the raw prefix arg, not a string or list containing additional
    ;; args.
    (unless (comint-check-proc (iex-buf-name))
      (set-buffer
       (apply 'make-comint iex-proc-name (or elixir-iex-command "iex") nil ())))
    (iex-switch-to-inf)))

(defun iex-switch-to-inf ()
  "Switch to the iex buffer. Signals an error if it does not exist."
  (interactive)
  (if (get-buffer (iex-buf-name))
      (pop-to-buffer (iex-buf-name))
    (error "No current iex buffer.")))

(when (and (locate-library "smie")
           (file-exists-p "~/.emacs.d/emacs-elixir/elixir-mode.el"))
  (add-to-list 'load-path "~/.emacs.d/emacs-elixir")
  (require 'elixir-mode)
  (add-hook 'elixir-mode-hook
            '(lambda ()
               (define-key elixir-mode-map "\C-cd" 'debug-comment)
               (define-key elixir-mode-map "\r" 'newline-and-indent)
               (define-key elixir-mode-map "\C-cr" 'executable-interpret)
               (define-key elixir-mode-map "\C-c\C-z" 'iex-switch-to-inf))))
