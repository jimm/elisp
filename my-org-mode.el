;;; Org Mode

(require 'org)
(require 'ox-md nil t)                  ; markdown export mode
(unless (boundp 'org-ans1)
  (defvar org-ans1)
  (defvar org-ans2))

(let ((emacs-mastery-base-dir (concat (getenv "writing") "/books/emacs-mastery/")))
  (setq org-publish-project-alias
        '(("emacs-mastery"
           :base-directory emacs-mastery-base-dir
           :publishing-directory (concat emacs-mastery-base-dir "_html/")))))

;;; Org Capture

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "todo.org" "To Do")
         "* TODO %?\n  %a\n  %f"
         :prepend t
         :empty-lines 1)
        ("n" "Note" entry (file+headline "notes.org" "Notes")
         "* %?\n  %U\n  %i\n  %a"
         :prepend t
         :empty-lines 1)))

;; Org Mode extras

(defun my-org-execute-src ()
  "Saves current Org mode src block to a temp file and executes
it in a compilation buffer by using the source language
name (e.g., \"sh\", \"ruby\") as a command. Obviously doesn't
work for all langauges."
  (interactive)
  (let* ((props (cadr (org-element-context)))
         (p-beg (1- (plist-get props :begin)))
         (p-end (1- (plist-get props :end)))
         (lang (plist-get props :language))
         (tmpfile (make-temp-file "org-src-")))
    (write-region p-beg p-end tmpfile)
    (compile (concat lang " " tmpfile))))

;; The first three are recommended
(setq org-agenda-include-diary t
      org-directory (concat *my-pim-dir* "orgs/")
      org-agenda-files (list (concat *my-pim-dir* "orgs/todo.org"))
      org-startup-folded 'nofold
      org-adapt-indentation nil
      org-src-fontify-natively t
      org-fontify-whole-heading-line t  ; bg color covers whole line
      org-default-notes-file (concat *my-pim-dir* "orgs/notes.org")
      org-use-sub-superscripts "{}")

(add-hook 'org-mode-hook
          (lambda ()
            (org-add-link-type "addr" #'address)
            (org-add-link-type "date" #'my-goto-calendar-date)
            (setq org-export-with-sub-superscripts nil)
            (define-key org-mode-map "\C-cr" #'my-org-execute-src)
            (define-key org-mode-map "\C-ct" #'org-toggle-link-display)

            ;; Bind keys normally used for TAGS finding/return
            (define-key org-mode-map "\M-." #'org-open-at-point)
            (define-key org-mode-map "\M-," #'org-mark-ring-goto)

            ;; yasnippet mode
            ;; TODO org-set-local has gone away. Delete this call when all
            ;; of my Emacs instances are updated
            (when-fboundp-call org-set-local 'yas-trigger-key "\t")
            (add-to-list 'org-tab-first-hook
                         (lambda ()
                           (let ((yas/fallback-behavior 'return-nil))
                             (yas/expand))))
            (when (fboundp #'yas-global-mode)
              (define-key yas-keymap "\t" 'yas-next-field-or-maybe-expand))))

;; Modify org-structure-template-alist when Org Mode version is < 9.3.
(when (version< org-version "9.3")
    "I like lower-case Org Mode templates. This function returns a
copy of org-structure-template-alist with lower-case template
values."
    (setq org-structure-template-alist
          (mapcar (lambda (entry)
                    (list (car entry)
                          (downcase (cadr entry))))
                  org-structure-template-alist)))

(when (>= emacs-major-version 24)
  (set-face-attribute 'org-level-1 nil
                      :height 1.15
                      :bold t)
  (set-face-attribute 'org-level-2 nil
                      :bold t))

;;; ================================================================

;;
;; Org Present Mode
;;

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-present-hide-cursor)
                 (org-display-inline-images)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-present-show-cursor)
                 (org-remove-inline-images)
                 (org-present-read-write)))))

;; Note that there's a bug in the (old) version of Org mode that Org Present
;; depends on that breaks table formatting.
;;
;; https://github.com/rlister/org-present
(defvar *org-present-orig-background* nil)
(add-hook 'org-present-mode-hook
          (lambda ()
            (set-background-color *org-presentation-background*)))
(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (set-background-color *current-background*)))
