;;; status.el
;;;
;;; Provides a way to store quick notes in an Org file. Status notes are
;;; grouped by date, most recent first.

(defvar *status-file*
  (concat user-emacs-directory "status.org")
  "The file to which status notes are written.")

(defun status-ensure-todays-date ()
  "Makes sure that the first line of the current buffer is a
  top-leve Org mode header containing today's date. Assumes the
  current buffer is open on *status-file*."
  (let ((date-header (concat "* " (format-time-string "%Y-%m-%d"))))
    (goto-char (point-min))
    (unless (looking-at (concat "* " date-header))
      (insert date-header "\n\n\n"))))

(defun status (status-str)
  "Insert STATUS-STR into the Org mode file *status-file*."
  (interactive "sStatus: ")
  (find-file *status-file*)
  (status-ensure-todays-date)
  (goto-char (point-min))
  (forward-line 2)
  (insert "- " status-str "\n")
  (forward-line -1))
