;;; ido.el --- interactively do things with buffers and files.

;; Copyright (C) 1996-2001  Free Software Foundation, Inc.

;; Author: Kim F. Storm <storm@cua.dk>
;; Based on: iswitchb by Stephen Eglen <stephen@cns.ed.ac.uk>
;; Maintainer: Kim F. Storm <storm@cua.dk>
;; Location: http://www.cua.dk/
;; Version: 1.56
;; Keywords: extensions convenience

;; This file is (not yet) part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Acknowledgements

;; Infinite amounts of gratitude goes to Stephen Eglen <stephen@cns.ed.ac.uk>
;; who wrote iswitch-buffer mode - from which I ripped off 99% of the code 
;; for ido-switch-buffer and found the inspiration for ido-find-file.
;; The ido package would never have existed without his work.

;; Also thanks to Klaus Berndl, Rohit Namjoshi, Robert Fenk, Alex Schroeder,
;; Bill Benedetto, and Stephen Eglen for bug fixes and improvements.

;;; History

;; Since I discovered Stephen Eglen's excellent iswitchb package, I just
;; couldn't live without it, but once being addicted to switching buffers
;; with a minimum of keystrokes, I soon found that opening files in the
;; old-fashioned way was just too slow - so I decided to write a package
;; which could open files with the same speed and ease as iswitchb could
;; switch buffers.

;; I originally wrote a separate ifindf.el package based on a copy of
;; iswitchb.el, which did for opening files what iswitchb did for
;; switching buffers.  Along the way, I corrected a few errors in
;; ifindf which could have found its way back into iswitchb, but since
;; most of the functionality of the two package was practically
;; identical, I decided that the proper thing to do was to merge my
;; ifindf package back into iswitchb.
;;
;; This is basically what ido (interactively do) is all about; but I
;; found it ackward to merge my changes into the "iswitchb-" namespace,
;; so I invented a common "ido-" namespace for the merged packages.
;;
;; Versions:
;; 1.20:    <klaus.berndl@sdm.de>
;;         + Added prefix macthing in addition to the normal substring-matching
;;           and a toggle function (`ido-enable-prefix' and `ido-toggle-prefix').
;;         + Added highlighting for sub-directories in the alternative-list
;;           for better and faster overview (`ido-subdir-face').
;;         + Added possibilty to customize for what function-group (buffer-handling
;;           or file-handling) ido should be enabled. Look at `ido-enabled'.
;;         + Removed some bugs:
;;           - file-functions (find, insert, write...) now handle correct the
;;             default-directory of the current buffer.
;;           - added compatiblity for emacs without customize for deface too.
;; 1.21:  + Added support for emacs 21.0
;; 1.22:  + Added ido-max-prospects to speed up prompting in big directories.
;; 1.23:  + Cleanup for public release
;; 1.24:  + Move visited files to end of file selection list
;;        + Save ido-last-directory-list in ~/.ido.last
;; 1.25:  + Move exact match to front of selection list (unless in prefix mode)
;; 1.26:  + Kill .ido.last buffer after loading it.
;; 1.27:  + Added ido-find-file-read-only from Rohit Namjoshi.
;;        + Added misc fixes from Robert Fenk.
;;        + Added ido-max-prompt-path.
;;        + Rotate file list to get default item in front [up-dir only]
;; 1.28:  + Added C-e to edit input string
;; 1.29:  + Added ido-dired [C-x d] and ido-list-directory [C-x C-d].
;;          If specifying a non-existing directory, offer to create it.
;; 1.30:  + Disable show-paren-mode in minibuffer
;;        + Dired now has . as default choice to make entering current directory easy.
;;        + Fixed handling of / in directory paths
;;        + "../" is now handled like backspace (updir).
;;        + C-f now always falls back to non-ido version of current command.
;;        + Handle $VAR expansion in file names
;;        + Fixed unintended ange-ftp interaction in root directory.
;; 1.31   + Changed location & contact e-mail address
;; 1.32   + Added fixes and enhancements from Klaus Berndl:
;;          - New ido-separator variable.
;;          - Fixed documentation of various make-list-hooks.
;;          - Don't backspace past root directory.
;;        + Fixed: default-directory of current buffer was changed by find-file fallback.
;;        + Added ido-make-file-prompt-hook and ido-rewrite-prompt-path-rules variables.
;;        + Added work directory history for ido-find-file.
;; 1.33   + When using multiple frames, ido-switch-buffer now shows current frame's buffers first.
;; 1.34   + Selection with keyboard or mouse now works in completion buffer.
;;          - New variable ido-completion-buffer (default *Ido Completion*).
;;          - Completion buffer is automatically removed when context changes.
;;          - Completion buffer contents are now sorted alphabetically.
;;        + Fixed: ido-dired didn't put directory in work directory history.
;;        + Added short description of how ido-find-file actually works.
;;        + [M-backspace] now deletes word (if any) or last subdir like [backspace]
;; 1.35   + Added work file history (on M-o and M-b)
;;        + Fixed: allow entering dired with wildcard filename.
;;        + C-w now inserts current buffer's file name.
;;        + More customization: ido-completion-buffer-all-completions
;;        + Work file and directory lists can now be saved and loaded using
;;          M-x ido-save-history and M-x ido-load-history.
;; 1.36   + Fixed handling of ~/ (was broken in 1.34) 
;;        + $VAR expansion how happens automatically when typing /
;;        + Fixed: dired would store /path/./ in work directory history.
;;        + ido-write-file now disables ido-work-directory-match-only.
;; 1.37   + Avoid corrupting .ido.last and check after load.
;; 1.40   + Added ido-work-directory-list-ignore-regexps.
;;        + Added ido-dir-file-cache and ido-max-dir-file-cache.
;;        + Added automatic lookup in work directory history for files when no match.
;;	  + Added ido-merge-work-directories and ido-auto-merge-work-directories-length.
;; 1.41   + Ignore text properties in minibuffer contents (if yanked)
;;	  + Fixed: don't auto-merge in root to allow entry of ange-ftp hostnames
;; 1.42   + Don't auto-merge in write-file, dired, and list-directory
;;        + Restore pre-merge state when backspacing or no merged matches found
;;	  + C-z now undoes last merge and returns to the pre-merge directory
;; 1.43   + Added ido-merged-indicator and ido-indicator-face.
;;	  + Use directory file cache rather than work directory list when merging directories (ending in /)
;; 1.44   + Use directory cache contents when creating merged directory list.
;;	  + Added ido-ignore-directories-merge.
;;	  + Fixed customize type for ignore lists to choice of regexps or functions.
;; 1.45   + Added C-o command to ido-find-file to copy file name from current buffer.
;;        + Modified C-w to no copy extension if current directory hasn't changed.
;; 1.46   + Added ido-auto-merge-delay to delay auto merge operation by 500 ms waiting for more input.
;;        + Corrected minor problems related to cycling through merged lists.
;; 1.47   + Added ido-decorations to allow more customization of ido minibuffer.
;;        + Enhanced ido-wash-history to remove text properties from strings.
;;        + In match list, strings whose prefix matches are now placed first.
;;        + Fixed: Clear previous input string when C-g is used to quit minibuffer.
;;        + Fixed: Restore previous matches when undoing merge using [backspace].
;;        + Fixed: Avoid duplicates in ido-work-file-list.
;;        + No longer uses pp to save ido history file.
;;        + Added ido-read-file-name and ido-read-directory-name functions.
;; 1.48   + In emacs 21, now resizes minibuffer according to max-mini-window-height.
;;        + Added ido-max-window-height to override max-mini-window-height.
;;        + Added fix from Alex Schroeder for ido-max-prompt-path customization.
;; 1.49   + Fixed: Input was not echoed in minibuffer when no matches; the fix
;;          uses a timer to perform auto-merge delay rather than sit-for.
;;        + Replaced ido-auto-merge-delay by ido-auto-merge-delay-time (seconds).
;;        + Don't auto-merge with 1 character only (so don't auto-merge on ~).
;;        + M-k removes current directory from ido-work-directory-list.
;; 1.50   + User input now interrupts auto merge.
;;        + Fixed: maintain current input when switching between file and buffer selection.
;; 1.51   + Fixed ido-to-end: would clear ido-temp-list if called with entire list.
;;        + C-backspace now goes to parent directory without clearing current file name.
;;        + Disable print-level and print-length limitations when saving history.
;;        + Now check proper formatting of all loaded history data in ido-wash-history.
;;          Discard bogus elements.
;;        + Added M-m to make-directory in current directory.
;;        + Added M-f to search for a matching files using external find command.
;;        + Added M-d to search for a matching directories using external find command.
;; 1.52   + Fixed: Ignore error messages from `find' command in ido-wide-find-dirs-or-files.
;;        + Added ido-slow-ftp-hosts and ido-slow-ftp-host-regexps to automatically fallback
;;          to standard find-file if visiting one of the matching hosts.
;;        + Added ido-merge-ftp-work-directories; by default, ftp hosts are ignored when
;;          searching work directories.
;;        + Added ido-cache-ftp-work-directory-time; by default, ftp host directories are
;;          cached for one hour without checking for new contents.
;;        + Don't cache root directories on nt and ms-dos systems.
;; 1.53   + Immediately fallback to non-ido find-file if ido-find-file is activated
;;          in a buffer whose default-directory is on a slow ftp host.
;;        + C-k in ido-find-file offers to delete file at head of list.
;; 1.54   + Adapted key-binding fix from Bill Benedetto for (ido-mode 'buffer).
;;        + Added find-file-read-only-other-window and find-file-read-only-other-frame.
;;        + Adapted fix from Stephen Eglen to use select-frame-set-input-focus when available.
;; 1.55   + Fixed ido-minibuffer-setup for xemacs (but ido still has problems is some areas with xemacs).
;;        + Automatically look in list of ignored files or buffers [C-a] when there are no matches.
;;          Specifically, .. is now completed normally although it is ignored by default.
;;        + Removed the ido-merge-work-directories variable which modified the functionality of M-p and M-n.
;;        + The M-p and M-n commands now always go to the previous / next (matching) work directory in the history.
;;        + New M-s command (ido-merge-work-directories) to actively search (and merge) the work directory history.
;;        + Added ido-show-dot-for-dired to allow easy entry to dired from ido-find-file.
;; 1.56   + Don't use obsoleted insert-string in emacs 21.3
;;        + Fixed: Don't bind backspace and M-backspace specifically for xemacs.
;;        

;;; Commentary:

;; Installation:
;; To get the alternative switch-to-buffer and find-file functions in
;; this package bound to keys, do
;;  (require 'ido)
;;  (ido-mode t)

;; Substring matching (The default method)
;;
;; As you type in a substring, the list of buffers or files currently
;; matching the substring are displayed as you type.  The list is
;; ordered so that the most recent buffers or files visited come at
;; the start of the list.
;; The buffer or file at the start of the list will be the one visited
;; when you press return.  By typing more of the substring, the list is
;; narrowed down so that gradually the buffer or file you want will be
;; at the top of the list.  Alternatively, you can use C-s and C-r (or
;; the right and left arrow keys) to rotate buffer or file names in the
;; list until the one you want is at the top of the list.
;; Completion is also available so that you can see what is common to
;; all of the matching buffers or files as you type.
;;
;; This code is based on the iswitchb package by Stephen Eglen, and 
;; large parts of the code and comments is copied directly from iswitchb
;; with only editorial changes on my part.
;;
;; Prefix matching
;;
;; The standard way of completion with Unix-shells and Emacs is to insert a
;; PREFIX and then hitting TAB (or another completion key). Cause of this
;; behavior has become second nature to a lot of emacs users `ido' offers in
;; addition to the default substring-matching-method (look above) also the
;; prefix-matching-method. The kind of matching is the only difference to
;; the description of the substring-matching above.
;; Prefix matching was added by Klaus Berndl (klaus.berndl@sdm.de) based on
;; an idea of Yuji Minejima <ggb01164@nifty.ne.jp> and his mcomplete-package.

;;; Example 

;; Substring matching
;;
;; If I have two buffers called "123456" and "123", with "123456" the
;; most recent, when I use ido-switch-buffer, I first of all get
;; presented with the list of all the buffers
;;
;;       Buffer:  {123456,123} 
;;
;; If I then press 2:
;;       Buffer: 2[3]{123456,123}
;;
;; The list in {} are the matching buffers, most recent first (buffers
;; visible in the current frame are put at the end of the list by
;; default).  At any time I can select the item at the head of the
;; list by pressing RET.  I can also bring the put the first element
;; at the end of the list by pressing C-s or [right], or put the last
;; element at the head of the list by pressing C-r or [left].
;; The item in [] indicates what can be added to my input by pressing TAB.
;; In this case, I will get "3" added to my input.  So, press TAB:
;;	 Buffer: 23{123456,123}
;;
;; At this point, I still have two matching buffers.
;; If I want the first buffer in the list, I simply press RET.  If I
;; wanted the second in the list, I could press C-s to move it to the
;; top of the list and then RET to select it.
;;
;; However, If I type 4, I only have one match left:
;;       Buffer: 234[123456] [Matched]
;;
;; Since there is only one matching buffer left, it is given in [] and we
;; see the text [Matched] afterwards.  I can now press TAB or RET to go
;; to that buffer.
;;
;; If however, I now type "a":
;;       Buffer: 234a [No match]
;; There are no matching buffers.  If I press RET or TAB, I can be
;; prompted to create a new buffer called "234a".
;;
;; Of course, where this function comes in really useful is when you
;; can specify the buffer using only a few keystrokes.  In the above
;; example, the quickest way to get to the "123456" file would be
;; just to type 4 and then RET (assuming there isn't any newer buffer
;; with 4 in its name).
;;
;; Prefix matching
;;
;; If you have again two Buffers "123456" and "123" then hitting "2" does
;; not match because "2" is not a PREFIX in any of the buffer-names. This
;; is the only difference between the substring- and prefix-matching.

;; Additional functionalty
;;
;; To see a full list of all matching buffer in a separate buffer,
;; hit ? or press TAB when there are no further completions to the
;; substring.  Repeated TAB presses will scroll you through this
;; separate buffer.

;; The buffer at the head of the list can be killed by pressing C-k.
;; If the buffer needs saving, you will be queried before the buffer
;; is killed.

;; If you find that the file you are after is not in a buffer, you can
;; press C-f to immediately drop into ido-find-file.

;; Likewise, if you use ido-find-file, the list of files and
;; directories in the current directory is provided in the same
;; fashion as the buffers above. However, the files and directories
;; are simply sorted in alphabetical order.
;;
;; In addition to scrolling through the list using [right] and [left],
;; you can use [up] and [down] to quickly scroll the list to the next
;; or previous subdirectory.
;;
;; To go down into a subdirectory, and continue the file selection on
;; the files in that directory, simply move it to the head of the list
;; and hit RET.
;;
;; To go up to the parent directory, delete any partial file name
;; already specified (e.g. using [backspace]) and hit [backspace].
;;
;; To go to the root directory (on the current drive), enter two slashes.
;; On MS-DOS or Windows, to select the root of another drive, enter X:/
;; where X is the drive letter.
;;
;; If for some reason you cannot specify the proper file using
;; ido-find-file, you can press C-f to enter the normal find-file.
;; You can also press C-b to drop into ido-switch-buffer.

;; See the doc string of ido-switch-buffer and ido-find-file for full
;; keybindings and features.
;;  (describe-function 'ido-find-file)

;;; Customisation

;; See the User Variables section below for easy ways to change the
;; functionality of the program.  These are accessible using the
;; custom package.
;; To modify the keybindings, use the hook provided.  For example:
;;(add-hook 'ido-define-mode-map-hook 'ido-my-keys)
;;
;;(defun ido-my-keys ()
;;  "Add my keybindings for ido."
;;  (define-key ido-mode-map " " 'ido-next-match)
;;  )
;;
;; Seeing all the matching files
;;
;; If you have many matching files, they may not all fit onto one
;; line of the minibuffer.  In this case, you should use rsz-mini
;; (resize-minibuffer-mode).  You can also limit ido so that it
;; only shows a certain number of lines -- see the documentation for
;; `ido-minibuffer-setup-hook'.

;; Changing the list of files

;; By default, the list of current files is most recent first,
;; oldest last, with the exception that the files visible in the
;; current frame are put at the end of the list.  A hook exists to
;; allow other functions to order the list.  For example, if you add:
;;
;; (add-hook 'ido-make-buffer-list-hook 'ido-summary-buffers-to-end)
;;
;; then all files matching "Summary" are moved to the end of the
;; list.  (I find this handy for keeping the INBOX Summary and so on
;; out of the way.)  It also moves files matching "output\*$" to the
;; end of the list (these are created by AUC TeX when compiling.)
;; Other functions could be made available which alter the list of
;; matching files (either deleting or rearranging elements.)

;; Highlighting

;; The highlighting of matching items is controlled via ido-use-faces.
;; The faces used are ido-first-match-face, ido-only-match-face and
;; ido-subdir-face.
;; Colouring of the matching item was suggested by
;; Carsten Dominik (dominik@strw.leidenuniv.nl).

;; Replacement for read-buffer

;; ido-read-buffer has been written to be a drop in replacement
;; for the normal buffer selection routine `read-buffer'.  To use
;; iswitch for all buffer selections in Emacs, add:
;; (setq read-buffer-function 'ido-read-buffer)
;; (This variable was introduced in Emacs 20.3)
;; XEmacs users can get the same behaviour by doing:
;; (defalias 'read-buffer 'ido-read-buffer) 
;; since `read-buffer' is defined in lisp.

;; Regexp matching

;; There is limited provision for regexp matching within ido,
;; enabled through `ido-enable-regexp'.  This allows you to type `c$'
;; for example and see all file names ending in `c'.  This facility
;; is quite limited though in two respects.  First, you can't
;; currently type in expressions like `[0-9]' directly -- you have to
;; type them in when ido-enable-regexp is nil and then toggle on the
;; regexp functionality.  Likewise, don't enter an expression
;; containing `\' in regexp mode.  If you try, ido gets confused,
;; so just hit C-g and try again.  Secondly, no completion mechanism
;; is currently offered when regexp searching.


;;; Code:

(provide 'ido)

;; CL needed for cadr and last
(if (not (and (fboundp 'cadr)
	      (fboundp 'last)))
    (require 'cl))

;; Set up the custom library.
;; taken from http://www.dina.kvl.dk/~abraham/custom/
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (unless (fboundp 'defgroup)
    (defmacro defgroup (&rest rest) ()))
  (unless (fboundp 'defcustom)
    (defmacro defcustom (sym val str &rest rest) `(defvar ,sym ,val ,str)))
  (unless (fboundp 'defface)
    (defmacro defface (sym val str &rest rest)
      `(defvar ,sym (make-face ',sym) ,str)))
  (unless (fboundp 'minibuffer-prompt-end)
    (defun minibuffer-prompt-end () (point-min))))

;;; User Variables
;;
;; These are some things you might want to change.

(defun ido-fractionp (n)
  (and (numberp n) (> n 0.0) (<= n 1.0)))

(defgroup ido nil
  "Switch between files using substrings."
  :group 'extensions
  :group 'convenience
  :link '(emacs-commentary-link :tag "Commentary" "ido.el")
  :link '(emacs-library-link :tag "Lisp File" "ido.el"))

;;;###autoload
(defcustom ido-enabled nil
  "Determines for which functional group \(buffer and files) ido behavior
should be enabled. The following values are possible:
- 'buffer: Turn only on ido buffer behavior \(switching, killing,
  displaying...) 
- 'file: Turn only on ido file behavior \(finding, writing, inserting...)
- 'both: Turn on ido buffer and file behavior.
- nil: Turn off any ido switching.

Setting this variable directly does not take effect;
use either \\[customize] or the function `ido-mode'."
  :set #'(lambda (symbol value)
	   (ido-mode value))
  :initialize 'custom-initialize-default
  :require 'ido
  :link '(emacs-commentary-link "ido.el")
  ;;  :version "20.5"
  :type '(choice (const :tag "Turn on only buffer" buffer) 
                 (const :tag "Turn on only file" file)
                 (const :tag "Turn on both buffer and file" both)
                 (const :tag "Switch off all" nil))
  :group 'ido)

(defcustom ido-case-fold case-fold-search
  "*Non-nil if searching of buffer and file names should ignore case."
  :type 'boolean
  :group 'ido)

(defcustom ido-ignore-buffers
  '("\\` ")
  "*List of regexps or functions matching buffer names to ignore.
For example, traditional behavior is not to list buffers whose names begin
with a space, for which the regexp is `\\` '.  See the source file for
example functions that filter buffernames."
  :type '(repeat (choice regexp function))
  :group 'ido)

(defcustom ido-ignore-files
  '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./")
  "*List of regexps or functions matching file names to ignore.
For example, traditional behavior is not to list files whose names begin
with a #, for which the regexp is `\\`#'.  See the source file for
example functions that filter filenames."
  :type '(repeat (choice regexp function))
  :group 'ido)

(defcustom ido-ignore-extensions t
  "*Non-nil means ignore files in completion-ignored-extensions list."
  :type 'boolean
  :group 'ido)

(defcustom ido-show-dot-for-dired nil
  "*Non-nil means to always put . as the first item in file name lists.
This allows the current directory to be opened immediate with `dired'."
  :type 'boolean
  :group 'ido)

(defcustom ido-ignore-directories
  '("\\`CVS/" "\\`\\.\\./" "\\`\\./")
  "*List of regexps or functions matching sub-directory names to ignore."
  :type '(repeat (choice regexp function))
  :group 'ido)

(defcustom ido-ignore-directories-merge nil
  "*List of regexps or functions matching directory path names to ignore during merge.
Directory paths matched by one of the regexps in this list are not inserted
in merged file and directory lists."
  :type '(repeat (choice regexp function))
  :group 'ido)

;;; Examples for setting the value of ido-ignore-buffers
;(defun ido-ignore-c-mode (name)
;  "Ignore all c mode buffers -- example function for ido."
;  (save-excursion
;    (set-buffer name)
;    (string-match "^C$" mode-name)))
;
;(setq ido-ignore-buffers '("^ " ido-ignore-c-mode))

;;; Examples for setting the value of ido-ignore-files
;(setq ido-ignore-files '("^ " "\\.c$" "\\.h$"))

(defcustom ido-default-file-method  'always-frame
    "*How to switch to new file when using `ido-find-file'.
Possible values:
`samewindow'	Show new file in same window
`otherwindow'	Show new file in another window (same frame)
`display'	Display file in another window without switching to it
`otherframe'	Show new file in another frame
`maybe-frame'	If a file is visible in another frame, prompt to ask if you
		you want to see the file in the same window of the current
  		frame or in the other frame.
`always-frame'  If a file is visible in another frame, raise that
		frame.  Otherwise, visit the file in the same window."
    :type '(choice (const samewindow) 
		   (const otherwindow)
		   (const display)
		   (const otherframe) 
		   (const maybe-frame)
		   (const always-frame))
    :group 'ido)

(defcustom ido-default-buffer-method  'always-frame
    "*How to switch to new buffer when using `ido-switch-buffer'.
See ido-default-file-method for details."
    :type '(choice (const samewindow) 
		   (const otherwindow)
		   (const display)
		   (const otherframe) 
		   (const maybe-frame)
		   (const always-frame))
    :group 'ido)

(defcustom ido-enable-regexp nil
  "*Non-nil means that `ido' will do regexp matching.
Value can be toggled within `ido' using `ido-toggle-regexp'."
  :type 'boolean
  :group 'ido)

(defcustom ido-enable-prefix nil
  "*Nil means that `ido' will match if the inserted text is an
arbitrary substring (default). If non-nil `ido' will only match if the inserted
text is a prefix \(this behavior is like the standard unix- or
emacs-completion works).
Value can be toggled within `ido' using `ido-toggle-prefix'."
  :type 'boolean
  :group 'ido)

(defcustom ido-record-commands t
  "*Non-nil means that `ido' will record commands in command history.
Note that the non-ido equivalent command is recorded."
  :type 'boolean
  :group 'ido)

(defcustom ido-max-prospects 12
  "*Non-zero means that the prospect list will be limited to than number of items.
For a long list of prospects, building the full list for the minibuffer can take a
non-negletable amount of time; setting this variable reduces that time."
  :type 'integer
  :group 'ido)

(defcustom ido-max-prompt-path 0.35
  "*Non-zero means that the prompt string be limited to than number of characters.
If value is a floating point number, it specifies a fraction of the frame width."
  :type '(choice
	  (integer :tag "Characters" :value 20)
	  (restricted-sexp :tag "Fraction of frame width"
			   :value 0.35
			   :match-alternatives (ido-fractionp)))
  :group 'ido)

(defcustom ido-max-window-height nil
  "*Non-nil specifies a value to override `max-mini-window-height'."
  :type '(choice
	  (const :tag "Don't override" nil)
	  (integer :tag "Number of lines" :value 1)
	  (restricted-sexp
	   :tag "Fraction of window height"
	   :value 0.25
	   :match-alternatives (ido-fractionp)))
  :group 'ido)

(defcustom ido-enable-last-directory-history t
  "*Non-nil means that `ido' will remember latest selected directory paths.
See `ido-last-directory-list' and `ido-save-directory-list-file'."
  :type 'boolean
  :group 'ido)

(defcustom ido-max-work-directory-list 50
  "*Maximum number of working directories to record.
This is the list of directories where files have most recently been opened.
See `ido-work-directory-list' and `ido-save-directory-list-file'."
  :type 'integer
  :group 'ido)

(defcustom ido-work-directory-list-ignore-regexps nil
  "*List of regexps matching directories which should not be recorded.
Directory paths matched by one of the regexps in this list are not inserted in
the `ido-work-directory-list' list."
  :type '(repeat regexp)
  :group 'ido)

(defcustom ido-record-ftp-work-directories t
  "*Non-nil means that ftp paths are recorded in work directory list."
  :type 'boolean
  :group 'ido)

(defcustom ido-merge-ftp-work-directories nil
  "*Nil means that ftp paths in work directory list are ignored during merge."
  :type 'boolean
  :group 'ido)

(defcustom ido-cache-ftp-work-directory-time 1.0
  "*Maximum time to cache contents of an ftp directory (in hours).
If zero, ftp directories are not cached."
  :type 'number
  :group 'ido)

(defcustom ido-slow-ftp-hosts nil
  "*List of slow ftp hosts where ido prompting should not be used.
If an ftp host is on this list, ido automatically switches to the non-ido
equivalent function, e.g. find-file rather than ido-find-file."
  :type '(repeat string)
  :group 'ido)

(defcustom ido-slow-ftp-host-regexps nil
  "*List of regexps matching slow ftp hosts (see `ido-slow-ftp-hosts')."
  :type '(repeat regexp)
  :group 'ido)

(defcustom ido-max-work-file-list 10
  "*Maximum number of names of recently opened files to record.
This is the list the file names (sans directory) which have most recently
been opened. See `ido-work-file-list' and `ido-save-directory-list-file'."
  :type 'integer
  :group 'ido)

(defcustom ido-work-directory-match-only t
  "*Non-nil means to skip non-matching directories in the directory history.
When some text is already entered at the `ido-find-file' prompt, using
\\[ido-prev-work-directory] or \\[ido-next-work-directory] will skip directories
without any matching entries."
  :type 'boolean
  :group 'ido)

(defcustom ido-auto-merge-work-directories-length 0
  "*Automatically switch to merged work directories during file name input.
The value is number of characters to type before switching to merged mode.
If zero, the switch happens when no matches are found in the current directory.
Automatic merging is disabled if the value is negative."
  :type 'integer
  :group 'ido)

(defcustom ido-auto-merge-delay-time 0.70
  "*Delay in seconds to wait for more input before doing auto merge."
  :type 'number
  :group 'ido)

(defcustom ido-merged-indicator "^"
  "The string appended to first choice if it has multiple directory choices."
  :type 'string
  :group 'ido)

(defcustom ido-max-dir-file-cache 100
  "*Maximum number of working directories to be cached.
This is a cache of file-name-all-completions results.
See `ido-dir-file-cache' and `ido-save-directory-list-file'."
  :type 'integer
  :group 'ido)

(defcustom ido-rotate-file-list-default nil
  "*Non-nil means that `ido' will always rotate file list to get default in front."
  :type 'boolean
  :group 'ido)

(defcustom ido-create-new-buffer 'prompt
  "*Specify whether a new buffer is created if no buffer matches substring.
Choices are 'always to create new buffers unconditionally, 'prompt to
ask user whether to create buffer, or 'never to never create new buffer."
  :type '(choice (const always) 
		 (const prompt)
		 (const never))
  :group 'ido)

(defcustom ido-define-mode-map-hook  nil
  "*Hook to define keys in `ido-mode-map' for extra keybindings."
  :type 'hook
  :group 'ido)

(defcustom ido-separator nil
  "*String used by ido to separate the alternatives in the minibuffer.
Obsolete.  Set 3rd element of `ido-decorations' instead."
  :type 'string
  :group 'ido)

(defcustom ido-decorations '( "{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]")
  "*List of strings used by ido to display the alternatives in the minibuffer.
There are 8 elements in this list, each is a pair of strings:
1st and 2nd elements are used as brackets around the prospect list,
3rd element is the separator between prospects (ignored if ido-separator is set),
4th element is the string inserted at the end of a truncated list of prospects,
5th and 6th elements are used as brackets around the common match string which
can be completed using TAB,
7th element is the string displayed when there are a no matches, and 
8th element displayed if there is a single match (and faces are not used)."
  :type '(repeat string)
  :group 'ido)

(defcustom ido-use-faces t
  "*Non-nil means use ido faces to highlighting first match, only match and
subdirs in the alternatives."
  :type 'boolean
  :group 'ido)

(defface ido-first-match-face  '((t (:bold t)))
  "*Font used by ido for highlighting first match."
  :group 'ido)

(defface ido-only-match-face  '((((class color)) 
                                 (:foreground "ForestGreen"))
                                (t (:italic t)))
  "*Font used by ido for highlighting only match."
  :group 'ido)

(defface ido-subdir-face  '((((class color)) 
                             (:foreground "red"))
                            (t (:underline t)))
  "*Font used by ido for highlighting subdirs in the alternatives."
  :group 'ido)

(defface ido-indicator-face  '((((class color)) 
				(:foreground "yellow"
				 :background "red"
				 :width condensed))
			       (t (:inverse-video t)))
  "*Font used by ido for highlighting its indicators."
  :group 'ido)

(defcustom ido-make-file-list-hook  nil
  "*List of functions to run when the list of matching files is created.
Each function on the list may modify the dynamically bound variable
`ido-temp-list' which contains the current list of matching files."
  :type 'hook
  :group 'ido)

(defcustom ido-make-dir-list-hook  nil
  "*List of functions to run when the list of matching directories is created.
Each function on the list may modify the dynamically bound variable
`ido-temp-list' which contains the current list of matching directories."
  :type 'hook
  :group 'ido)

(defcustom ido-make-buffer-list-hook  nil
  "*List of functions to run when the list of matching buffers is created.
Each function on the list may modify the dynamically bound variable
`ido-temp-list' which contains the current list of matching buffer names."
  :type 'hook
  :group 'ido)

(defcustom ido-make-file-prompt-hook nil
  "*List of functions to run when the find-file prompt is created.
Each function on the list may modify the following dynamically bound
variables:
  path   - the (abbreviated) directory path
  max-width - the max width of the path; set to nil to inhibit truncation
  prompt - the basic prompt (e.g. \"Find File: \")
  literal - the string shown if doing `literal' find; set to nil to omit
  vc-off  - the string shown if version control is inhibited; set to nit to omit
  prefix  - normally nil, but may be set to a fixed prefix for the path
The following variables are available, but should not be changed:
  ido-current-directory - the unabbreviated directory path
  item - equals 'file or 'dir depending on the current mode."
  :type 'hook
  :group 'ido)

(defvar ido-rewrite-prompt-path-rules nil
  "*Alist of rewriting rules for file paths.
A list of elements of the form (FROM . TO) or (FROM . FUNC),
each meaning to rewrite the path if matched by FROM by either
substituting the matched string by TO or calling the function
FUNC with the current path as its only argument and using the
return value as the new path.  In addition, each FUNC may
also modify the dynamic variables described for the
variable `ido-make-file-prompt-hook'.")

(defcustom ido-completion-buffer "*Ido Completions*"
  "*Name of completion buffer used by ido.
Set to nil to disable completion buffers popping up."
  :type 'string
  :group 'ido)

(defcustom ido-completion-buffer-all-completions nil
  "*Non-nil means to show all completions in completion buffer.
Otherwise, only the current list of matches is shown."
  :type 'boolean
  :group 'ido)

(defvar ido-all-frames 'visible
  "*Argument to pass to `walk-windows' when finding visible files.
See documentation of `walk-windows' for useful values.")

(defcustom ido-minibuffer-setup-hook nil
  "*Ido-specific customization of minibuffer setup.

This hook is run during minibuffer setup iff `ido' will be active.
It is intended for use in customizing ido for interoperation
with other packages.  For instance:

  \(add-hook 'ido-minibuffer-setup-hook 
	    \(function
	     \(lambda ()
	       \(make-local-variable 'resize-minibuffer-window-max-height)
	       \(setq resize-minibuffer-window-max-height 3))))

will constrain rsz-mini to a maximum minibuffer height of 3 lines when
ido is running.  Copied from `icomplete-minibuffer-setup-hook'."
  :type 'hook
  :group 'ido)

(defvar ido-save-directory-list-file "~/.ido.last"
  "File in which the ido state is saved between invocations.
Variables stored are: ido-last-directory-list and ido-work-directory-list.
Must be set before enabling ido mode.")

;;; Internal Variables

;; Persistent variables

(defvar ido-mode-map nil
  "Keymap for `ido-find-file' and `ido-switch-buffer'.")

(defvar  ido-file-history nil
  "History of files selected using `ido-find-file'.")

(defvar  ido-buffer-history nil
  "History of buffers selected using `ido-switch-buffer'.")

(defvar ido-xemacs  (string-match "XEmacs" (emacs-version))
  "Non-nil if we are running XEmacs.  Otherwise, assume we are running Emacs.")

(defvar ido-last-directory-list nil
  "List of last selected directory paths.
See `ido-enable-last-directory-history' for details.")

(defvar ido-work-directory-list nil
  "List of actual working directory paths.
The current directory is inserted at the front of this list whenever a
file is opened with ido-find-file and family.")

(defvar ido-work-file-list nil
  "List of actual work file names.
The current file name (sans path) is inserted at the front of this list
whenever a file is opened with ido-find-file and family.")

(defvar ido-dir-file-cache nil
  "List of file-name-all-completions results.
Each element in the list is of the form (dir (mtime) file...).")

;; Temporary storage

(defvar ido-eoinput 1
  "Point where minibuffer input ends and completion info begins.
Copied from `icomplete-eoinput'.")
(make-variable-buffer-local 'ido-eoinput)

(defvar ido-common-match-string  nil
  "Stores the string that is common to all matching files.")

(defvar ido-rescan nil
  "Non-nil means we need to regenerate the list of matching items.")

(defvar ido-rotate nil
  "Non-nil means we are rotating list of matches.")

(defvar ido-text nil
  "Stores the users string as it is typed in.")

(defvar ido-text-init nil
  "The initial string for the users string it is typed in.")

(defvar ido-matches nil
  "List of files currently matching `ido-text'.")

(defvar ido-report-no-match t
  "Report [No Match] when no completions matches ido-text.")

(defvar ido-exit nil 
  "Flag to monitor how `ido-find-file' exits.  
If equal to `takeprompt', we use the prompt as the file name to be
selected.")

(defvar ido-current-directory nil
  "Current directory for ido-find-file.")

(defvar ido-auto-merge-timer nil
  "Delay timer for auto merge.")

;; The following variables are dynamic variables created by ido,
;; but they are declared here to keep the byte compiler quiet.

(eval-when-compile
  (defvar ido-cur-item nil
    "Stores the current ido item type ('file, 'dir or 'buffer).")

  (defvar ido-cur-list nil
    "Stores the current list of items that will be searched through.
The list is ordered, so that the most interesting item comes first,
although by default, the files visible in the current frame are put
at the end of the list.  Created by `ido-make-item-list'.")

  (defvar ido-ignored-list nil
    "Stores the list of items which are ignored when building `ido-cur-list'.
It is in no specific order.")

  (defvar ido-keep-item-list nil
    "Keep current item list if non-nil")

  (defvar ido-use-mycompletion-depth 0
    "Non-nil means use `ido' completion feedback.  
Is set by ido functions to the current minibuffer-depth, so that
it doesn't interfere with other minibuffer usage.")

  (defvar ido-process-ignore-lists t
    "Process ido-ignore- lists.")

  (defvar ido-process-ignore-lists-inhibit nil
    "Don't process ido-ignore- lists once.")

  (defvar ido-default-item nil
    "Default item for ido.")

  (defvar ido-entry-buffer nil
    "Buffer from which ido was entered.")

  (defvar ido-require-match nil
    "Non-nil if matching file must be selected.")

  (defvar ido-temp-list nil
    "Stores a temporary version of the file list being created.")

  (defvar ido-rotate-temp nil
    "Non-nil if default list element should be rotated into place.")

  (defvar ido-work-directory-index nil
    "Stores current index in ido-work-directory-list.")

  (defvar ido-use-merged-list nil
    "Set when merged work directory list is in use.")

  (defvar ido-try-merged-list t
    "Set when merged work directory list not yet built.")

  (defvar ido-pre-merge-state nil
    "Saved state prior to last work directory merge.
Value is a list (ido-text dir cur-list ignored-list matches).")

  (defvar ido-work-file-index nil
    "Stores current index in ido-work-file-list.")

  (defvar ido-bufs-in-frame nil
    "List of the files visible in the current frame.")

  (defvar ido-change-word-sub nil 
    "Private variable used by `ido-word-matching-substring'.")

  (defvar ido-saved-vc-mt nil
    "Original value of vc-master-templates for use in ido-toggle-vc.")

  (defvar ido-find-literal nil
    "Stores temporary state of literal find file.")
)

;;; FUNCTIONS

(defun ido-active (&optional merge)
  (if merge
      ido-use-merged-list
    (and (boundp 'ido-completing-read) (= ido-use-mycompletion-depth (minibuffer-depth)))))

(defvar ido-trace-enable nil)

(defun ido-trace (p &optional s retval)
  (if ido-trace-enable
      (let ((b (get-buffer-create " *IDO Trace*"))
	    (deactivate-mark deactivate-mark))
	(save-excursion
	  (save-restriction
	    (set-buffer b)
	    (insert p ": " (if (stringp s) s (format "%S" s)) "\n")))))
  retval)

(defun ido-is-root-directory (&optional dir)
  (setq dir (or dir ido-current-directory))
  (if (memq system-type '(windows-nt ms-dos))
      (string-match "\\`[a-zA-Z]:[/\\]\\'" dir)
    (string-equal "/" dir)))

(defun ido-is-ftp-directory (&optional dir)
  (string-match "\\`/[^/:][^/:]+:/" (or dir ido-current-directory)))

(defun ido-is-slow-ftp-host (&optional dir)
  (and (or ido-slow-ftp-hosts ido-slow-ftp-host-regexps)
       (setq dir (or dir ido-current-directory))
       ;; (featurep 'ange-ftp)
       ;; (ange-ftp-ftp-name dir)
       (string-match "\\`/\\([^/:]*@\\)?\\([^@/:][^@/:]+\\):/" dir)
       (let ((host (substring dir (match-beginning 2) (match-end 2))))
	 (or (member host ido-slow-ftp-hosts)
	     (let ((re ido-slow-ftp-host-regexps))
	       (while (and re (not (string-match (car re) host)))
		 (setq re (cdr re)))
	       re)))))

(defun ido-time-stamp (&optional time)
  ;; Time is a floating point number (fractions of 1 hour)
  (setq time (or time (current-time)))
  (/ (+ (* (car time) 65536.0) (car (cdr time))) 3600.0))

(defun ido-cache-ftp-valid (&optional time)
  (and (numberp ido-cache-ftp-work-directory-time)
       (> ido-cache-ftp-work-directory-time 0)
       (or (not time)
	   (< (- (ido-time-stamp) time) ido-cache-ftp-work-directory-time))))

(defun ido-may-cache-directory (&optional dir)
  (setq dir (or dir ido-current-directory))
  (if (and (memq system-type '(windows-nt ms-dos))
	   (string-match "\\`[a-zA-Z]:[/\\]\\'" dir))
      nil
    (or (not (ido-is-ftp-directory dir))
	(ido-cache-ftp-valid))))

(defun ido-pp (list &optional sep)
  (let ((print-level nil) (eval-expression-print-level nil)
	(print-length nil) (eval-expression-print-length nil))
    (insert "\n;; ----- " (symbol-name list) " -----\n(\n ")
    (setq list (symbol-value list))
    (while list
      (let* ((elt (car list))
	     (s (if (consp elt) (car elt) elt)))
	(if (and (stringp s) (= (length s) 0))
	    (setq s nil))
	(if s
	    (prin1 elt (current-buffer)))
	(if (and (setq list (cdr list)) s)
	    (insert (or sep "\n ")))))
    (insert "\n)\n")))

(defun ido-save-history ()
  "Save ido history and cache information between sessions."
  (interactive)
  (if (and ido-last-directory-list ido-save-directory-list-file)
      (save-excursion
	(save-window-excursion
	  (if (find-buffer-visiting ido-save-directory-list-file)
	      (kill-buffer (find-buffer-visiting ido-save-directory-list-file)))
	  (if (file-exists-p ido-save-directory-list-file)
	      (delete-file ido-save-directory-list-file))
	  (set-buffer (let ((enable-local-variables nil))
			(find-file-noselect ido-save-directory-list-file t)))
	  (goto-char (point-min))
	  (delete-region (point-min) (point-max))
	  (ido-pp 'ido-last-directory-list)
	  (ido-pp 'ido-work-directory-list)
	  (ido-pp 'ido-work-file-list)
	  (ido-pp 'ido-dir-file-cache "\n\n ")
	  (insert "\n")
	  (let ((version-control 'never))
	    (write-file ido-save-directory-list-file nil))
	  (kill-buffer (current-buffer))))))

(defun ido-load-history (&optional arg)
  "Load ido history and cache information from previous session.
With prefix argument, reload history unconditionally."
  (interactive "P")
  (if (or arg (and ido-save-directory-list-file (not ido-last-directory-list)))
      (let ((file (expand-file-name ido-save-directory-list-file))
	    buf)
	(when (file-readable-p file)
	  (save-excursion
	    (save-window-excursion
	      (setq buf (set-buffer (let ((enable-local-variables nil))
				      (find-file-noselect file))))
	      (goto-char (point-min))
	      (condition-case nil
		  (setq ido-last-directory-list (read (current-buffer))
			ido-work-directory-list (read (current-buffer))
			ido-work-file-list (read (current-buffer))
			ido-dir-file-cache (read (current-buffer)))
		(error nil))))
	  (kill-buffer buf))))
  (ido-wash-history))

(defun ido-wash-history ()
  "Clean-up ido history and cache information.
Removes badly formatted data and ignored directories."
  (interactive)
  ;; Check format of each of our lists, discard bogus elements
  (setq ido-last-directory-list
	(and (listp ido-last-directory-list)
	     (let ((l ido-last-directory-list) r)
	       (while l
		 (if (and (consp (car l))
			  (stringp (car (car l)))
			  (stringp (cdr (car l))))
		     (setq r (cons (car l) r)))
		 (setq l (cdr l)))
	       (nreverse r))))
  (setq ido-work-directory-list 
	(and (listp ido-work-directory-list)
	     (let ((l ido-work-directory-list) r)
	       (while l
		 (if (and (stringp (car l))
			  (or ido-record-ftp-work-directories
			      (not (ido-is-ftp-directory (car l)))))
		     (setq r (cons (car l) r)))
		 (setq l (cdr l)))
	       (nreverse r))))
  (setq ido-work-file-list 
	(and (listp ido-work-file-list)
	     (let ((l ido-work-file-list) r)
	       (while l
		 (if (stringp (car l))
		     (setq r (cons (car l) r)))
		 (setq l (cdr l)))
	       (nreverse r))))
  (setq ido-dir-file-cache 
	(and (listp ido-dir-file-cache)
	     (let ((l ido-dir-file-cache) r)
	       (while l
		 (if (and (listp (car l))
			  (> (length (car l)) 2)
			  (let ((dir (car (car l)))
				(time (car (cdr (car l))))
				(files (cdr (cdr (car l)))))
			    (and
			     (stringp dir)
			     (consp time)
			     (if (integerp (car time))
				 (and (/= (car time) 0)
				      (integerp (car (cdr time)))
				      (/= (car (cdr time)) 0)
				      (ido-may-cache-directory dir))
			       (and (eq (car time) 'ftp)
				    (numberp (cdr time))
				    (ido-is-ftp-directory dir)
				    (ido-cache-ftp-valid (cdr time))))
			     (let ((s files) (ok t))
			       (while s
				 (if (stringp (car s))
				     (setq s (cdr s))
				   (setq s nil ok nil)))
			       ok))))
		     (setq r (cons (car l) r)))
		 (setq l (cdr l)))
	       (nreverse r))))

  ;; Remove ignored directories from work directory list
  ;; according to ido-work-directory-list-ignore-regexps
  (if ido-work-directory-list
      (let ((dirs (reverse ido-work-directory-list)))
	(setq ido-work-directory-list nil)
	(while dirs
	  (ido-record-work-directory (car dirs))
	  (setq dirs (cdr dirs)))))
  ;; Get rid of text properties
  (let ((l ido-last-directory-list) e)
    (while l
      (setq e (car l) l (cdr l))
      (set-text-properties 0 (length (car e)) nil (car e))
      (set-text-properties 0 (length (cdr e)) nil (cdr e))))
  (let ((l ido-work-directory-list) e)
    (while l
      (setq e (car l) l (cdr l))
      (set-text-properties 0 (length e) nil e)))
  (let ((l ido-work-file-list) e)
    (while l
      (setq e (car l) l (cdr l))
      (set-text-properties 0 (length e) nil e)))
  (let ((l ido-dir-file-cache) e d)
    (while l
      (setq e (car l) l (cdr l))
      (if (listp e)
	  (while e
	    (setq d (car e) e (cdr e))
	    (if (not (consp d))
		(set-text-properties 0 (length d) nil d))))))
)


(defun ido-kill-emacs-hook ()
  ;; ido kill emacs hook
  (ido-save-history))

;;;###autoload
(defun ido-mode (&optional arg nobind)
  "Toggle ido speed-ups on or off.
With ARG, turn ido speed-up on if arg is positive, off otherwise.
If second argument NOBIND is non-nil, no keys are rebound; otherwise,
turning on ido-mode will modify the default keybindings for the 
find-file and switch-to-buffer families of commands to the ido
versions of these functions.
However, if second arg equals 'files, bind only for files, or if it 
equals 'buffers, bind only for buffers.
This function also adds a hook to the minibuffer."
  (interactive "P")
  (setq ido-enabled
	(cond 
	 ((null arg) (if ido-enabled nil 'both))
	 ((eq arg t) 'both)
	 ((eq arg 'files) 'file)
	 ((eq arg 'buffers) 'buffer)
	 ((memq arg '(file buffer both)) arg)
	 ((> (prefix-numeric-value arg) 0) 'both)
	 (t nil)))
  (when ido-enabled
    (add-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)
    (ido-load-history)

    (add-hook 'kill-emacs-hook 'ido-kill-emacs-hook)

    (when (memq ido-enabled '(file both))
      (define-key ctl-x-map "\C-f"   'ido-find-file)
      (define-key ctl-x-map "\C-r"   'ido-find-file-read-only)
      (define-key ctl-x-map "\C-v"   'ido-find-alternate-file)
      (define-key ctl-x-map "\C-w"   'ido-write-file)
      (define-key ctl-x-map "i"      'ido-insert-file)
      (define-key ctl-x-map "\C-d"   'ido-list-directory)
      (define-key ctl-x-map "d"	     'ido-dired)

      (define-key ctl-x-4-map "f"    'ido-find-file-other-window)
      (define-key ctl-x-4-map "\C-f" 'ido-find-file-other-window)
      (define-key ctl-x-4-map "r"    'ido-find-file-read-only-other-window)

      (define-key ctl-x-5-map "f"    'ido-find-file-other-frame)
      (define-key ctl-x-5-map "\C-f" 'ido-find-file-other-frame)
      (define-key ctl-x-5-map "r"    'ido-find-file-read-only-other-frame))

    (when (memq ido-enabled '(buffer both))
      (define-key ctl-x-map "b"      'ido-switch-buffer)
      (define-key ctl-x-map "\C-i"   'ido-insert-buffer)
      (define-key ctl-x-map "k"      'ido-kill-buffer)

      (define-key ctl-x-4-map "b"    'ido-switch-buffer-other-window)
      (define-key ctl-x-4-map "\C-o" 'ido-display-buffer)

      (define-key ctl-x-5-map "b"    'ido-switch-buffer-other-frame)
      )))


;;; IDO KEYMAP 
(defun ido-define-mode-map ()
  "Set up the keymap for `ido'."
  (let (map)
    ;; generated every time so that it can inherit new functions.

    (setq map (copy-keymap minibuffer-local-map))
    (define-key map "\C-a" 'ido-toggle-ignore)
    (define-key map "\C-c" 'ido-toggle-case)
    (define-key map "\C-e" 'ido-edit-input)
    (define-key map "\t" 'ido-complete)
    (define-key map "\C-j" 'ido-select-text)
    (define-key map "\C-m" 'ido-exit-minibuffer)
    (define-key map "\C-p" 'ido-toggle-prefix)
    (define-key map "\C-r" 'ido-prev-match)
    (define-key map "\C-s" 'ido-next-match)
    (define-key map "\C-t" 'ido-toggle-regexp)
    (define-key map "\C-z" 'ido-undo-merge-work-directory)
    (define-key map [right] 'ido-next-match)
    (define-key map [left] 'ido-prev-match)
    (define-key map "?" 'ido-completion-help)

    (when (memq ido-cur-item '(file dir))
      (define-key map "\C-b" 'ido-enter-switch-buffer)
      (define-key map "\C-d" 'ido-enter-dired)
      (define-key map "\C-f" 'ido-fallback-command)
      (define-key map [down] 'ido-next-match-dir)
      (define-key map [up]   'ido-prev-match-dir)
      (define-key map [(meta up)] 'ido-prev-work-directory)
      (define-key map [(meta down)] 'ido-next-work-directory)
      (define-key map [backspace] 'ido-delete-backward-updir)
      (define-key map "\d"        'ido-delete-backward-updir)
      (define-key map [(meta backspace)] 'ido-delete-backward-word-updir)
      (define-key map [(control backspace)] 'ido-up-directory)
      (define-key map [(meta ?b)] 'ido-next-work-file)
      (define-key map [(meta ?d)] 'ido-wide-find-dir)
      (define-key map [(meta ?f)] 'ido-wide-find-file)
      (define-key map [(meta ?k)] 'ido-forget-work-directory)
      (define-key map [(meta ?m)] 'ido-make-directory)
      (define-key map [(meta ?n)] 'ido-next-work-directory)
      (define-key map [(meta ?o)] 'ido-prev-work-file)
      (define-key map [(meta ?p)] 'ido-prev-work-directory)
      (define-key map [(meta ?s)] 'ido-merge-work-directories)
      )

    (when (eq ido-cur-item 'file)
      (define-key map "\C-k" 'ido-delete-file-at-head)
      (define-key map "\C-l" 'ido-toggle-literal)
      (define-key map "\C-o" 'ido-copy-current-word)
      (define-key map "\C-v" 'ido-toggle-vc)
      (define-key map "\C-w" 'ido-copy-current-file-name)
      )

    (when (eq ido-cur-item 'buffer)
      (define-key map "\C-b" 'ido-fallback-command)
      (define-key map "\C-f" 'ido-enter-find-file)
      (define-key map "\C-k" 'ido-kill-buffer-at-head)
      )

    (setq ido-mode-map map)
    (run-hooks 'ido-define-mode-map-hook)))

(defun ido-final-slash (dir &optional fix-it)
  ;; return DIR if DIR has final slash.
  ;; else if FIX-IT is non-nil, return DIR/
  ;; else return nil.
  (setq dir (ido-name dir))
  (cond
   ((string-match "/\\'" dir) dir)
   (fix-it (concat dir "/"))
   (t nil)))

(defun ido-set-current-directory (dir &optional subdir no-merge)
  ;; Set ido's current directory to DIR or DIR/SUBDIR
  (setq dir (ido-final-slash dir t))
  (setq ido-use-merged-list nil
	ido-try-merged-list (not no-merge))
  (if subdir
      (setq dir (ido-final-slash (concat dir subdir) t)))
  (if (equal dir ido-current-directory)
      nil
    (ido-trace "cd" dir)
    (setq ido-current-directory dir)
    (if (get-buffer ido-completion-buffer)
	(kill-buffer ido-completion-buffer))
    t))

(defun ido-set-current-home ()
  ;; Set ido's current directory to user's home directory
  (ido-set-current-directory (expand-file-name "~/")))

(defun ido-record-command (command arg)
  ;; Add (command arg) to command-history if ido-record-commands is t
  (if ido-record-commands
      (let ((cmd (list command arg)))
	(if (or (not command-history)
		(not (equal cmd (car command-history))))
	    (setq command-history (cons cmd command-history))))))

(defun ido-make-prompt (item prompt)
  ;; Make the prompt for ido-read-internal
  (cond
   ((and (memq item '(file dir)) ido-current-directory)
    (let ((path (abbreviate-file-name ido-current-directory))
	  (max-width (if (and ido-max-prompt-path (floatp ido-max-prompt-path))
			 (floor (* (frame-width) ido-max-prompt-path))
		       ido-max-prompt-path))
	  (literal (and (boundp 'ido-find-literal) ido-find-literal "(literal) "))
	  (vc-off (and ido-saved-vc-mt (not vc-master-templates) "[-VC] "))
	  (prefix nil)
	  (rule ido-rewrite-prompt-path-rules))
      (let ((case-fold-search nil))
	(while rule
	  (if (and (consp (car rule))
		   (string-match (car (car rule)) path))
	      (setq path
		    (if (stringp (cdr (car rule)))
			(replace-match (cdr (car rule)) t nil path)
		      (funcall (cdr (car rule)) path))))
	  (setq rule (cdr rule))))
      (run-hooks 'ido-make-file-prompt-hook)
      (concat prompt 
	      ; (if ido-process-ignore-lists "" "&")
	      (or literal "")
	      (or vc-off  "")
	      (or prefix "")
	      (let ((l (length path)))
		(if (and max-width (> max-width 0) (> l max-width))
		    (let* ((s (substring path (- max-width))) 
			   (i (string-match "/" s)))
		      (concat "..." (if i (substring s i) s)))
		  path)))))
   (t prompt)))

;; Here is very briefly how ido-find-file works:
;;
;;  (ido-find-file)
;;    (ido-file-internal method)
;;       set ido-current-directory
;;       (ido-read-internal 'file ...)
;;          (while ...
;;             (ido-make-item-list ...)
;;             (ido-set-matches)
;;             (completing-read ... ido-text-init ...)
;;
;;               ... here user is allowed to type characters and commands
;;                   a command may set ido-exit and call (exit-minibuffer)
;;                   to make ido-read-internal do advanced tasks (or return)
;;
;;               ... ido-tidy and ido-exhibit are pre- and post-hooks
;;                   which are run before and after each user command.
;;
;;             return value from completing-read is stored in ido-final-text
;;             - ido-exit may cause further actions to be taken:
;;               'refresh - repeat loop (make-item-list, set-matches)
;;               'edit    - edit the prompt string, then repeat loop
;;               'keep    - repeat loop but don't (re)make-item-list
;;               'updir   - go up one directory, repeat loop
;;               else set ido-selected based on ido-final-text,
;;               optionally update ido-current-directory and repeat loop, or
;;               exit with the return value of ido-selected (file name)
;;       selected file name is returned from ido-read-internal,
;;       ido-exit and method determines what action is taken
;;       e.g. the file name may be ignored or joined with ido-current-directory, and
;;       the relevant function is called (find-file, write-file, etc).

(defun ido-read-internal (item prompt history &optional default require-match initial)
  "Perform the ido-read-buffer and ido-read-filename functions.
Return the name of a buffer or file selected.  
PROMPT is the prompt to give to the user.
DEFAULT if given is the default directory to start with.
If REQUIRE-MATCH is non-nil, an existing file must be selected.
If INITIAL is non-nil, it specifies the initial input string."
  (let
      ((ido-cur-item item)
       (ido-entry-buffer (current-buffer))
       (ido-process-ignore-lists t)
       (ido-process-ignore-lists-inhibit nil)
       (ido-set-default-item t)
       ido-selected
       ido-final-text
       (done nil)
       (icomplete-mode nil) ;; prevent icomplete starting up
       ;; Exported dynamic variables:
       ido-cur-list
       ido-ignored-list
       (ido-rotate-temp nil)
       (ido-keep-item-list nil)
       (ido-use-merged-list nil)
       (ido-try-merged-list t)
       (ido-pre-merge-state nil)
       (ido-case-fold ido-case-fold)
       (ido-enable-prefix ido-enable-prefix)
       (ido-enable-regexp ido-enable-regexp)
       )

    (ido-define-mode-map)
    (setq ido-text-init initial)
    (while (not done)
      (ido-trace "\n_LOOP_")
      (setq ido-exit nil)
      (setq ido-rescan t)
      (setq ido-rotate nil)
      (setq ido-text "")
      (if ido-set-default-item
       (setq ido-default-item
	     (cond
	      ((eq item 'buffer)
	       (if (bufferp default) (buffer-name default) default))
	      ((eq item 'file)
	       (and ido-enable-last-directory-history 
		    (let ((d (assoc ido-current-directory ido-last-directory-list)))
		      (and d (cdr d))))))
	     ido-set-default-item nil))

      (if ido-process-ignore-lists-inhibit
	  (setq ido-process-ignore-lists nil))

      (if (and ido-use-merged-list (memq ido-try-merged-list '(t wide)) (not ido-keep-item-list))
	  (let ((olist ido-cur-list)
		(oign ido-ignored-list)
		(omat ido-matches)
		(l (ido-make-merged-file-list ido-text-init
					      (eq ido-use-merged-list 'auto)
					      (eq ido-try-merged-list 'wide))))
	    (cond
	     ((not l)
	      (if (eq ido-try-merged-list 'wide)
		  (setq ido-pre-merge-state
			(list "" ido-current-directory olist oign omat)
			ido-cur-list nil
			ido-ignored-list nil
			ido-matches nil
			ido-keep-item-list t
			ido-try-merged-list (if (eq ido-use-merged-list 'auto) 'auto nil)
			ido-use-merged-list nil)
		(setq ido-cur-list olist
		      ido-ignored-list oign
		      ido-matches omat
		      ido-keep-item-list t
		      ido-try-merged-list (if (eq ido-use-merged-list 'auto) 'auto nil)
		      ido-use-merged-list nil)))
	     ((eq l t)
	      (setq ido-use-merged-list nil))
	     (t
	      (setq ido-pre-merge-state
		    (list ido-text-init ido-current-directory olist oign omat))
	      (ido-set-current-directory (car (cdr (car l))))
	      (if (ido-final-slash ido-text-init)
		  (setq ido-text-init ""))
	      (setq ido-cur-list l
		    ido-ignored-list nil
		    ido-matches l
		    ido-rescan nil
		    ido-keep-item-list t
		    ido-use-merged-list t)
	      (ido-trace "Merged" t)
	      ))))
      
      (cond
       (ido-keep-item-list
	(setq ido-keep-item-list nil
	      ido-rescan nil))
       ((eq ido-cur-item 'file)
	(setq ido-ignored-list nil
	      ido-cur-list (ido-make-file-list ido-default-item)))
       ((eq ido-cur-item 'dir)
	(setq ido-ignored-list nil
	      ido-cur-list (ido-make-dir-list ido-default-item)))
       ((eq ido-cur-item 'buffer)
	(setq ido-ignored-list nil
	      ido-cur-list (ido-make-buffer-list ido-default-item)))
       (t nil))
      (setq ido-rotate-temp nil)

      (if ido-process-ignore-lists-inhibit
	  (setq ido-process-ignore-lists t
		ido-process-ignore-lists-inhibit nil))

      (ido-set-matches)
      (if (and ido-matches (eq ido-try-merged-list 'auto))
	  (setq ido-try-merged-list t))
      (let 
	  ((minibuffer-local-completion-map ido-mode-map)
	   (max-mini-window-height (or ido-max-window-height
				       (and (boundp 'max-mini-window-height) max-mini-window-height)))
	   (ido-completing-read t)
	   (ido-require-match require-match)
	   (ido-use-mycompletion-depth (1+ (minibuffer-depth)))
	   (show-paren-mode nil))
	;; prompt the user for the file name
	(setq ido-exit nil)
	(setq ido-final-text
	      (catch 'ido
		(completing-read 
		 (ido-make-prompt item prompt)
		 '(("dummy".1)) nil nil ; table predicate require-match
		 (prog1 ido-text-init (setq ido-text-init nil))	;initial-contents
		 history))))
      (ido-trace "completing-read" ido-final-text)
      (if (get-buffer ido-completion-buffer)
	  (kill-buffer ido-completion-buffer))

      (ido-trace "\n_EXIT_" ido-exit)

      (cond
       ((eq ido-exit 'refresh)
	(if (and (eq ido-use-merged-list 'auto) 
		 (or (input-pending-p)))
	    (setq ido-use-merged-list nil
		  ido-keep-item-list t))
	nil)

       ((eq ido-exit 'done)
	(setq done t
	      ido-selected ido-text
	      ido-exit nil))

       ((memq ido-exit '(edit chdir))
	(cond 
	 ((memq ido-cur-item '(file dir))
	  (let* ((process-environment (cons "HOME=/" process-environment)) ;; cheat read-file-name
		 (edit (eq ido-exit 'edit))
		 (d ido-current-directory)
		 (f ido-text-init)
		 (path t))
	    (setq ido-text-init "")
	    (while path
	      (setq path (if edit
			     (read-file-name (concat prompt "[EDIT] ") d (concat d f) nil f)
			   f)
		    d (or (file-name-directory path) "/")
		    f (file-name-nondirectory path)
		    edit t)
	      (if (or 
		   (file-directory-p d)
		   (and (yes-or-no-p (format "Create directory %s? " d))
			(condition-case nil 
			    (progn (make-directory d t) t)
			  (error
			   (message "Could not create directory")
			   (sit-for 1)
			   nil))))
		  (progn
		    (ido-set-current-directory d nil (eq ido-exit 'chdir))
		    (setq ido-text-init f
			  path nil))))))
	 (t
	  (setq ido-text-init nil)
	  (setq ido-text-init (read-string (concat prompt "[EDIT] ") ido-final-text))))
	nil)

       ((eq ido-exit 'keep)
	(setq ido-keep-item-list t))

       ((memq ido-exit '(dired fallback findfile findbuffer))
	(setq done t))

       ((eq ido-exit 'updir)
	;; cannot go up if already at the root-dir (Unix) or at the
	;; root-dir of a certain drive (Windows or MS-DOS).
        (or (ido-is-root-directory)
	    (ido-set-current-directory (file-name-directory (substring ido-current-directory 0 -1))))
	(setq ido-set-default-item t))

       ;; Handling the require-match must be done in a better way.
       ((and require-match (not (ido-existing-item-p)))
	(error "must specify valid item"))

       (t
	(setq ido-selected
	      (if (and ido-matches (equal ido-final-text ""))
		  (ido-name (car ido-matches))	;; possibly choose the default file
		(if (or (eq ido-exit 'takeprompt)
			(null ido-matches))
		    ido-final-text
		  ;; else take head of list
		  (ido-name (car ido-matches)))))

	(cond
	 ((eq item 'buffer)
	  (setq done t))

	 ((string-equal "./" ido-selected)
	  nil)

	 ((string-equal "../" ido-selected)
	  ;; cannot go up if already at the root-dir (Unix) or at the
	  ;; root-dir of a certain drive (Windows or MS-DOS).
	  (or (ido-is-root-directory)
	      (ido-set-current-directory (file-name-directory (substring ido-current-directory 0 -1))))
	  (setq ido-set-default-item t))

	 ((and (string-equal ido-current-directory "/")
	       (string-match "..:\\'" ido-selected)) ;; Ange-ftp 
	  (ido-set-current-directory "/" ido-selected)
	  (if (ido-is-slow-ftp-host)
	      (setq ido-exit 'fallback
		    done t)
	    (setq ido-set-default-item t)))

	 ((or (string-match "[/\\][^/\\]" ido-selected)
	      (and (memq system-type '(windows-nt ms-dos))
		   (string-match "\\`.:" ido-selected)))
	  (ido-set-current-directory (file-name-directory ido-selected))
	  (setq ido-set-default-item t))

	 ((string-match "\\`~" ido-selected)
	  (ido-set-current-home))

	 ((ido-final-slash ido-selected)
	  (if ido-enable-last-directory-history
	      (let ((x (assoc ido-current-directory ido-last-directory-list)))
		(if x
		    (setcdr x ido-selected)
		  (setq ido-last-directory-list
			(cons (cons ido-current-directory ido-selected) ido-last-directory-list)))))
	  (ido-set-current-directory ido-current-directory ido-selected)
	  (setq ido-set-default-item t))

	 (t
	  (setq done t))))))
    ido-selected))

(defun ido-edit-input ()
  "Edit ido path and input string. Terminate by RET."
  (interactive)
  (setq ido-text-init ido-text)
  (setq ido-exit 'edit)
  (exit-minibuffer))

;;; MAIN FUNCTIONS
(defun ido-buffer-internal (method &optional fallback prompt default initial)
  ;; Internal function for ido-switch-buffer and friends
  (if (not ido-enabled)
      (call-interactively (or fallback 'switch-to-buffer))
    (let ((buf (ido-read-buffer (or prompt "Buffer: ") default nil initial)))

      ;; Choose the buffer name: either the text typed in, or the head
      ;; of the list of matches

      (cond 
       ((eq ido-exit 'findfile)
	(ido-file-internal ido-default-file-method nil nil nil nil ido-text))

       ((eq ido-exit 'fallback)
	(call-interactively (or fallback 'switch-to-buffer)))

       ;; Check buf is non-nil.
       ((not buf) nil)

       ;; View buffer if it exists
       ((get-buffer buf)
	(if (eq method 'insert)
	    (progn
	      (ido-record-command 'insert-buffer buf)
	      (insert-buffer buf))
	  (ido-visit-buffer buf method t)))

       ;; buffer doesn't exist
       ((eq ido-create-new-buffer 'never)
	(message "no buffer matching `%s'" buf))

       ((and (eq ido-create-new-buffer 'prompt)
	     (not (y-or-n-p (format "No buffer matching `%s', create one? " buf))))
	nil)

       ;; create a new buffer
       (t
	(setq buf (get-buffer-create buf))
	(if (fboundp 'set-buffer-major-mode)
	    (set-buffer-major-mode buf))
	(ido-visit-buffer buf method t))))))

;;;###autoload
(defun ido-read-buffer (prompt &optional default require-match initial)
  "Replacement for the built-in `read-buffer'.
Return the name of a buffer selected.  
PROMPT is the prompt to give to the user.  DEFAULT if given is the default
buffer to be selected, which will go to the front of the list.
If REQUIRE-MATCH is non-nil, an existing-buffer must be selected.
If INITIAL is non-nil, it specifies the initial input string."
  (let ((ido-current-directory nil))
    (ido-read-internal 'buffer prompt 'ido-buffer-history default require-match initial)))

(defun ido-record-work-directory (&optional dir)
  (when (and (numberp ido-max-work-directory-list) (> ido-max-work-directory-list 0))
    (if (and (setq dir (or dir ido-current-directory)) (> (length dir) 0))
	(let ((items ido-work-directory-list-ignore-regexps)
	      (case-fold-search nil))
	  (while (and items dir)
	    (if (string-match (car items) dir)
		(setq dir nil))
	    (setq items (cdr items)))
	  (if dir
	      (setq ido-work-directory-list (cons dir (delete dir ido-work-directory-list))))))
    (if (> (length ido-work-directory-list) ido-max-work-directory-list)
	(setcdr (nthcdr (1- ido-max-work-directory-list) ido-work-directory-list) nil))))

(defun ido-forget-work-directory ()
  (interactive)
  (when (and ido-current-directory ido-work-directory-list)
    (setq ido-work-directory-list (delete ido-current-directory ido-work-directory-list))
    (when ido-use-merged-list
      (ido-undo-merge-work-directory)
      (setq ido-exit 'refresh
	    ido-try-merged-list t
	    ido-use-merged-list t
	    ido-text-init ido-text
	    ido-rotate-temp t)
      (exit-minibuffer))))
  
(defun ido-record-work-file (name)
  ;; Save NAME in ido-work-file-list
  (when (and (numberp ido-max-work-file-list) (> ido-max-work-file-list 0))
    (or
     (and ido-work-file-list (equal (car ido-work-file-list) name))
     (setq ido-work-file-list (cons name (delete name ido-work-file-list))))
    (if (> (length ido-work-file-list) ido-max-work-file-list)
	(setcdr (nthcdr (1- ido-max-work-file-list) ido-work-file-list) nil))))

(defun ido-file-internal (method &optional fallback default prompt item initial)
  ;; Internal function for ido-find-file and friends
  (let ((ido-current-directory (expand-file-name (or default default-directory)))
	filename)

    (if (or (not ido-enabled) (ido-is-slow-ftp-host))
	(setq filename t
	      ido-exit 'fallback))

    (let (ido-saved-vc-mt
	  (vc-master-templates (and (boundp 'vc-master-templates) vc-master-templates))
	  (ido-work-directory-index -1)
	  (ido-work-file-index -1)
       	  (ido-find-literal nil))

      (unless filename
	(setq ido-saved-vc-mt vc-master-templates)
	(setq filename (ido-read-internal (or item 'file)
					  (or prompt "Find file: ")
					  'ido-file-history default nil initial)))

      ;; Choose the file name: either the text typed in, or the head
      ;; of the list of matches

      (cond
       ((eq ido-exit 'fallback)
	;; Need to guard setting of default-directory here, since
	;; we don't want to change directory of current buffer.
	(with-temp-buffer
	  (setq default-directory ido-current-directory)
	  (call-interactively (or fallback 'find-file))))

       ((eq ido-exit 'findbuffer)
	(ido-buffer-internal ido-default-buffer-method nil nil nil ido-text))

       ((eq ido-exit 'dired)
	(dired (concat ido-current-directory (or ido-text ""))))

       ((eq method 'alt-file)
	(ido-record-work-file filename)
	(setq default-directory ido-current-directory)
	(ido-record-work-directory)
	(find-alternate-file filename))

       ((memq method '(dired list-directory))
	(if (equal filename ".")
	    (setq filename ""))
	(let* ((path (ido-final-slash (concat ido-current-directory filename) t))
	       (file (substring path 0 -1)))
	  (cond
	   ((file-directory-p path)
	    (ido-record-command method path)
	    (ido-record-work-directory path)
	    (funcall method path))
	   ((file-directory-p ido-current-directory)
	    (cond
	     ((file-exists-p file)
	      (ido-record-command method ido-current-directory)
	      (ido-record-work-directory)
	      (funcall method ido-current-directory)
	      (if (eq method 'dired)
		  (dired-goto-file (expand-file-name file))))
	     ((string-match "[[*?]" filename)
	      (setq  path (concat ido-current-directory filename))
	      (ido-record-command method path)
	      (ido-record-work-directory)
	      (funcall method path))
	     ((y-or-n-p (format "Directory %s does not exist. Create it " filename))
	      (ido-record-command method path)
	      (ido-record-work-directory path)
	      (make-directory-internal path)
	      (funcall method path))
	     (t
	      ;; put make-directory command on history
	      (ido-record-command 'make-directory path))))
	   (t (error "No such directory")))))

       ((eq method 'write)
	(ido-record-work-file filename)
	(setq default-directory ido-current-directory)
	(ido-record-command 'write-file (concat ido-current-directory filename))
	(ido-record-work-directory)
	(write-file filename))

       ((eq method 'read-only)
	(ido-record-work-file filename)
	(setq filename (concat ido-current-directory filename))
	(ido-record-command fallback filename)
	(ido-record-work-directory)
	(funcall fallback filename))

       ((eq method 'insert)
	(ido-record-work-file filename)
	(setq filename (concat ido-current-directory filename))
	(ido-record-command 
	 (if ido-find-literal 'insert-file-literally 'insert-file)
	 filename)
	(ido-record-work-directory)
	(if ido-find-literal
	    (insert-file-contents-literally filename)
	  (insert-file-contents filename)))

       (filename
	(ido-record-work-file filename)
	(setq filename (concat ido-current-directory filename))
	(ido-record-command 'find-file filename)
	(ido-record-work-directory)
	(ido-visit-buffer (find-file-noselect filename nil ido-find-literal) method))))))

(defun ido-existing-item-p ()
  ;; Return non-nil if there is a matching item
  (not (null ido-matches)))

;;; COMPLETION CODE

(defun ido-set-common-completion  ()
  ;; Find common completion of `ido-text' in `ido-matches'
  ;; The result is stored in `ido-common-match-string'
  (let* (val)
    (setq  ido-common-match-string nil)
    (if (and ido-matches
	     (not ido-enable-regexp) ;; testing
             (stringp ido-text)
             (> (length ido-text) 0))
        (if (setq val (ido-find-common-substring ido-matches ido-text))
            (setq ido-common-match-string val)))
    val))

(defun ido-complete ()
  "Try and complete the current pattern amongst the file names."
  (interactive)
  (let (res)
    (cond 
     ((and (memq ido-cur-item '(file dir))
	   (string-match "[$]" ido-text))
      (let ((evar (substitute-in-file-name (concat ido-current-directory ido-text))))
	(if (not (file-exists-p (file-name-directory evar)))
	    (message "Expansion generates non-existing directory path")
	  (if (file-directory-p evar)
	      (ido-set-current-directory evar)
	    (let ((d (or (file-name-directory evar) "/"))
		  (f (file-name-nondirectory evar)))
	      (when (file-directory-p d)
		  (ido-set-current-directory d)
		  (setq ido-text-init f))))
	  (setq ido-exit 'refresh)
	  (exit-minibuffer))))

     ((not ido-matches)
      (when ido-completion-buffer
	(setq this-command 'ido-completion-help)
	(ido-completion-help)))
	  
     ((= 1 (length ido-matches))
      ;; only one choice, so select it.
      (exit-minibuffer))
	  
     (t ;; else there could be some completions
      (setq res ido-common-match-string)
      (if (and (not (memq res '(t nil)))
	       (not (equal res ido-text)))
	  ;; found something to complete, so put it in the minibuffer.
	  (progn
	    ;; move exact match to front if not in prefix mode
	    (setq ido-rescan (not ido-enable-prefix))
	    (delete-region (minibuffer-prompt-end) (point))
	    (insert res))
	;; else nothing to complete
	(ido-completion-help)
	)))))

(defun ido-undo-merge-work-directory (&optional text try refresh)
  "Undo or redo last ido directory merge operation.
If no merge has yet taken place, toggle automatic merging option."
  (interactive)
  (cond
   (ido-pre-merge-state
    (ido-set-current-directory (nth 1 ido-pre-merge-state))
    (setq ido-text-init (or text (car ido-pre-merge-state))
	  ido-cur-list (nth 2 ido-pre-merge-state)
	  ido-ignored-list (nth 3 ido-pre-merge-state)
	  ido-matches (nth 4 ido-pre-merge-state)
	  ido-use-merged-list nil
	  ido-try-merged-list try
	  ido-keep-item-list (not refresh)
	  ido-rescan nil
	  ido-exit 'refresh
	  ido-pre-merge-state nil)
    (exit-minibuffer))
   (text
    nil)
   (ido-try-merged-list
    (setq ido-try-merged-list nil))
   (ido-matches
    (setq ido-try-merged-list t))
   ((not ido-use-merged-list)
    (ido-merge-work-directories))))
	    
;;; TOGGLE FUNCTIONS

(defun ido-toggle-case ()
  "Toggle the value of `ido-case-fold'."
  (interactive)
  (setq ido-case-fold (not ido-case-fold))
  ;; ask for list to be regenerated.
  (setq ido-rescan t))

(defun ido-toggle-regexp ()
  "Toggle the value of `ido-enable-regexp'."
  (interactive)
  (setq ido-enable-regexp (not ido-enable-regexp))
  ;; ask for list to be regenerated.
  (setq ido-rescan t))

(defun ido-toggle-prefix ()
  "Toggle the value of `ido-enable-prefix'."
  (interactive)
  (setq ido-enable-prefix (not ido-enable-prefix))
  ;; ask for list to be regenerated.
  (setq ido-rescan t))

(defun ido-toggle-ignore ()
  "Toggle ignoring files specified with `ido-ignore-files'."
  (interactive)
  (setq ido-process-ignore-lists (not ido-process-ignore-lists))
  (setq ido-text-init ido-text)
  (setq ido-exit 'refresh)
  (exit-minibuffer))

(defun ido-toggle-vc ()
  "Disable version control for this file."
  (interactive)
  (if (and ido-enabled (eq ido-cur-item 'file))
      (progn
	(setq vc-master-templates 
	      (if vc-master-templates nil ido-saved-vc-mt))
	(setq ido-text-init ido-text)
	(setq ido-exit 'keep)
	(exit-minibuffer))))

(defun ido-toggle-literal ()
  "Toggle literal reading of this file."
  (interactive)
  (if (and ido-enabled (eq ido-cur-item 'file))
      (progn
	(setq ido-find-literal (not ido-find-literal))
	(setq ido-text-init ido-text)
	(setq ido-exit 'keep)
	(exit-minibuffer))))

(defun ido-exit-minibuffer ()
  "Exit minibuffer, but make sure we have a match if one is needed."
  (interactive)
  (if (or (not ido-require-match)
	   (ido-existing-item-p))
      (throw 'exit nil)))

(defun ido-select-text ()
  "Select the buffer or file named by the prompt.
If no buffer or file exactly matching the prompt exists, maybe create a new one."
  (interactive)
  (setq ido-exit 'takeprompt)
  (exit-minibuffer))

(defun ido-fallback-command ()
  "Fallback to non-ido version of current command."
  (interactive)
  (setq ido-exit 'fallback)
  (exit-minibuffer))

(defun ido-enter-find-file ()
  "Drop into find-file from buffer switching."
  (interactive)
  (setq ido-exit 'findfile)
  (exit-minibuffer))

(defun ido-enter-switch-buffer ()
  "Drop into ido-switch-buffer from file switching."
  (interactive)
  (setq ido-exit 'findbuffer)
  (exit-minibuffer))

(defun ido-enter-dired ()
  "Drop into dired from file switching."
  (interactive)
  (setq ido-exit 'dired)
  (exit-minibuffer))


(defun ido-up-directory (&optional clear)
  "Go up one directory level."
  (interactive "P")
  (setq ido-text-init (if clear nil ido-text))
  (setq ido-exit 'updir)
  (setq ido-rotate-temp t)
  (exit-minibuffer))

(defun ido-delete-backward-updir (count)
  "Delete char backwards, or at beginning of buffer, go up one level."
  (interactive "P")
  (cond
   ((= (minibuffer-prompt-end) (point))
    (if (not count)
	(ido-up-directory t)))
   ((and ido-pre-merge-state (string-equal (car ido-pre-merge-state) ido-text))
    (ido-undo-merge-work-directory (substring ido-text 0 -1) t t))
   (t
    (delete-backward-char (prefix-numeric-value count)))))

(defun ido-delete-backward-word-updir (count)
  "Delete all chars backwards, or at beginning of buffer, go up one level."
  (interactive "P")
  (if (= (minibuffer-prompt-end) (point))
      (if (not count)
	  (ido-up-directory t))
    (backward-kill-word (prefix-numeric-value count))))

(defun ido-get-work-directory (&optional incr must-match)
  (let ((n (length ido-work-directory-list))
	(i ido-work-directory-index)
	(j 0)
	dir)
    (if (or (not ido-text) (= (length ido-text) 0))
	(setq must-match nil))
    (while (< j n)
      (setq i (+ i incr)
	    j (1+ j))
      (if (> incr 0)
	  (if (>= i n) (setq i 0))
	(if (< i 0) (setq i (1- n))))
      (setq dir (nth i ido-work-directory-list))
      (if (and dir
	       (not (equal dir ido-current-directory))
	       (file-directory-p dir)
	       (or (not must-match)
		   (ido-set-matches1
		    (if (eq ido-cur-item 'file)
			(ido-make-file-list1 dir)
		      (ido-make-dir-list1 dir)))))
	  (setq j n)
	(setq dir nil)))
    (if dir
	(setq ido-work-directory-index i))
    dir))

(defun ido-prev-work-directory ()
  "Change to next working directory in list."
  (interactive)
  (let ((dir (ido-get-work-directory 1 ido-work-directory-match-only)))
    (when dir
      (ido-set-current-directory dir)
      (setq ido-exit 'refresh)
      (setq ido-text-init ido-text)
      (setq ido-rotate-temp t)
      (exit-minibuffer))))

(defun ido-next-work-directory ()
  "Change to previous working directory in list."
  (interactive)
  (let ((dir (ido-get-work-directory -1 ido-work-directory-match-only)))
    (when dir
      (ido-set-current-directory dir)
      (setq ido-exit 'refresh)
      (setq ido-text-init ido-text)
      (setq ido-rotate-temp t)
      (exit-minibuffer))))

(defun ido-merge-work-directories ()
  "Search (and merge) work directories for files matching the current input string."
  (interactive)
  (setq ido-use-merged-list t ido-try-merged-list t)
  (setq ido-exit 'refresh)
  (setq ido-text-init ido-text)
  (setq ido-rotate-temp t)
  (exit-minibuffer))

(defun ido-wide-find-file (&optional file)
  "Prompt for FILE to search for using find, starting from current directory."
  (interactive)
  (unless file
    (setq file (read-string (concat "Wide find file: " ido-current-directory) ido-text)))
  (when (> (length file) 0)
    (setq ido-use-merged-list t ido-try-merged-list 'wide)
    (setq ido-exit 'refresh)
    (setq ido-text-init file)
    (setq ido-rotate-temp t)
    (exit-minibuffer)))

(defun ido-wide-find-dir (&optional dir)
  "Prompt for DIR to search for using find, starting from current directory."
  (interactive)
  (unless dir
    (setq dir (read-string (concat "Wide find directory: " ido-current-directory) ido-text)))
  (when (> (length dir) 0)
    (setq ido-use-merged-list t ido-try-merged-list 'wide)
    (setq ido-exit 'refresh)
    (setq ido-text-init (ido-final-slash dir t))
    (setq ido-rotate-temp t)
    (exit-minibuffer)))

(defun ido-make-directory (&optional dir)
  "Prompt for DIR to create in current directory."
  (interactive)
  (unless dir
    (setq dir (read-string (concat "Make directory: " ido-current-directory) ido-text)))
  (when (> (length dir) 0)
    (setq dir (concat ido-current-directory dir))
    (unless (file-exists-p dir)
      (make-directory dir t)
      (ido-set-current-directory dir)
      (setq ido-exit 'refresh)
      (setq ido-text-init nil)
      (setq ido-rotate-temp t)
      (exit-minibuffer))))

(defun ido-get-work-file (incr)
  (let ((n (length ido-work-file-list))
	(i (+ ido-work-file-index incr))
	name)
    (if (> incr 0)
	(if (>= i n) (setq i 0))
      (if (< i 0) (setq i (1- n))))
    (setq name (nth i ido-work-file-list))
    (setq ido-work-file-index i)
    name))

(defun ido-prev-work-file ()
  "Change to next working file name in list."
  (interactive)
  (let ((name (ido-get-work-file 1)))
    (when name
      (setq ido-text-init name)
      (setq ido-exit 'refresh)
      (exit-minibuffer))))

(defun ido-next-work-file ()
  "Change to previous working file name in list."
  (interactive)
  (let ((name (ido-get-work-file -1)))
    (when name
      (setq ido-text-init name)
      (setq ido-exit 'refresh)
      (exit-minibuffer))))

(defun ido-copy-current-file-name (all)
  "Insert file name of current buffer.
If repeated, insert text from buffer instead."
  (interactive "P")
  (let* ((path (buffer-file-name ido-entry-buffer))
	 (name (and path (file-name-nondirectory path))))
    (when name
      (setq ido-text-init 
	    (if (or all 
		    (not (equal (file-name-directory path) ido-current-directory))
		    (not (string-match "\\.[^.]*\\'" name)))
		name
	    (substring name 0 (1+ (match-beginning 0)))))
      (setq ido-exit 'refresh
	    ido-try-merged-list nil)
      (exit-minibuffer))))
  
(defun ido-copy-current-word (all)
  "Insert current word (file name or path) from current buffer."
  (interactive "P")
  (let ((word (save-excursion
		(set-buffer ido-entry-buffer)
		(let ((p (point)) start-line end-line start-name name)
		  (beginning-of-line)
		  (setq start-line (point))
		  (end-of-line)
		  (setq end-line (point))
		  (goto-char p)
		  (if (re-search-backward "[^-_a-zA-Z0-9:./\\~@]" start-line 1)
		      (forward-char 1))
		  (setq start-name (point))
		  (re-search-forward "[-_a-zA-Z0-9:./\\~@]*" end-line 1)
		  (if (= start-name (point))
		      nil
		    (buffer-substring-no-properties start-name (point)))))))
    (if (cond
	 ((not word) nil)
	 ((string-match "\\`[~/]" word)
	  (setq ido-text-init word
		ido-try-merged-list nil
		ido-exit 'chdir))
	 ((string-match "/" word)
	  (setq ido-text-init (concat ido-current-directory word)
		ido-try-merged-list nil
		ido-exit 'chdir))
	 (t
	  (setq ido-text-init word
		ido-try-merged-list nil
		ido-exit 'refresh)))
	(exit-minibuffer))))

(defun ido-next-match () 
  "Put first element of `ido-matches' at the end of the list."
  (interactive)
  (if ido-matches
      (let ((next (cadr ido-matches)))
	(setq ido-cur-list (ido-chop ido-cur-list next))
	(setq ido-rescan t)
	(setq ido-rotate t))))

(defun ido-prev-match () 
  "Put last element of `ido-matches' at the front of the list."
  (interactive)
  (if ido-matches
      (let ((prev (car (last ido-matches))))
	(setq ido-cur-list (ido-chop ido-cur-list prev))
	(setq ido-rescan t)
	(setq ido-rotate t))))

(defun ido-next-match-dir () 
  "Find next directory in match list.
If work directories have been merged, cycle through directories for
first matching file."
  (interactive)
  (if ido-use-merged-list
      (if ido-matches
	  (let* ((elt (car ido-matches))
		 (dirs (cdr elt)))
	    (when (> (length dirs) 1)
	      (setcdr elt (ido-chop dirs (cadr dirs))))
	    (setq ido-rescan nil)))
    (let ((cnt (length ido-matches))
	  (i 1))
      (while (and (< i cnt) (not (ido-final-slash (nth i ido-matches))))
	(setq i (1+ i)))
      (if (< i cnt)
	  (setq ido-cur-list (ido-chop ido-cur-list (nth i ido-matches)))))))

(defun ido-prev-match-dir () 
  "Find previous directory in match list.
If work directories have been merged, cycle through directories
for first matching file."
  (interactive)
  (if ido-use-merged-list
      (if ido-matches
	  (let* ((elt (car ido-matches))
		 (dirs (cdr elt)))
	    (when (> (length dirs) 1)
	      (setcdr elt (ido-chop dirs (car (last dirs)))))
	    (setq ido-rescan nil)))
    (let* ((cnt (length ido-matches))
	   (i (1- cnt)))
      (while (and (> i 0) (not (ido-final-slash (nth i ido-matches))))
	(setq i (1- i)))
      (if (> i 0)
	  (setq ido-cur-list (ido-chop ido-cur-list (nth i ido-matches)))))))

(defun ido-chop (items elem)
  "Remove all elements before ELEM and put them at the end of ITEMS."
  (let ((ret nil)
	(next nil)
	(sofar nil))
    (while (not ret)
      (setq next (car items))
      (if (equal next elem)
	  (setq ret (append items (nreverse sofar)))
	;; else
	(progn
	  (setq items (cdr items))
	  (setq sofar (cons next sofar)))))
    ret))

(defun ido-name (item)
  ;; Return file name for current item, whether in a normal list
  ;; or a merged work directory list.
  (if (consp item) (car item) item))


;;; CREATE LIST OF ALL CURRENT FILES

(defun ido-all-completions ()
  ;; Return unsorted list of all competions.
  (let ((ido-process-ignore-lists nil))
    (cond
     ((eq ido-cur-item 'file)
      (ido-make-file-list1 ido-current-directory))
     ((eq ido-cur-item 'dir)
      (ido-make-dir-list1 ido-current-directory))
     ((eq ido-cur-item 'buffer)
      (ido-make-buffer-list1))
     (t nil))))


(defun ido-sort-list (items)
  ;; Simple list of file or buffer names
  (sort items (lambda (a b) (string-lessp a b))))

(defun ido-sort-merged-list (items promote)
  ;; Input is list of ("file" . "dir") cons cells.
  ;; Output is sorted list of ("file "dir" ...) lists
  (let ((l (sort items (lambda (a b) (string-lessp (car b) (car a)))))
	res a cur dirs)
    (while l
      (setq a (car l)
	    l (cdr l))
      (if (and res (string-equal (car (car res)) (car a)))
	  (progn
	    (setcdr (car (if cur (cdr res) res)) (cons (cdr a) (cdr (car res))))
	    (if (and promote (string-equal ido-current-directory (cdr a)))
		(setq cur t)))
	(setq res (cons (list (car a) (cdr a)) res)
	      cur nil)))
    res))

(defun ido-wide-find-dirs-or-files (dir file &optional prefix finddir)
  ;; As ido-run-find-command, but returns a list of cons pairs ("file" . "dir")
  (let ((paths 
	 (split-string 
	  (shell-command-to-string
	   (concat "find " dir " -name \"" (if prefix "" "*") file "*\" -type " (if finddir "d" "f") " -print"))))
	path d f
	res)
    (while paths
      (setq path (car paths)
	    paths (cdr paths)
	    paths (cdr paths))
      (if (and (string-match "^/" path)
	       (file-exists-p path))
	  (setq d (file-name-directory path)
		f (file-name-nondirectory path)
		res (cons (cons (if finddir (ido-final-slash f t) f) d) res))))
    res))

(defun ido-flatten-merged-list (items)
  ;; Create a list of path names based on a merged directory list.
  (let (res)
    (while items
      (let* ((item (car items))
	     (file (car item))
	     (dirs (cdr item)))
	(while dirs
	  (setq res (cons (concat (car dirs) file) res)
		dirs (cdr dirs))))
      (setq items (cdr items)))
    res))

(defun ido-make-merged-file-list (text auto wide)
  (let (res)
    (message "Searching for `%s'...." text)
    (if (and (ido-final-slash text) ido-dir-file-cache)
	(if wide
	    (setq res (ido-wide-find-dirs-or-files
		       ido-current-directory (substring text 0 -1) ido-enable-prefix t))
	  ;; Use list of cached directories
	  (let ((re (concat (regexp-quote (substring text 0 -1)) "[^/:]*/\\'"))
		(dirs ido-dir-file-cache)
		dir b d f)
	    (if nil ;; simple
		(while dirs
		  (setq dir (car (car dirs))
			dirs (cdr dirs))
		  (when (and (string-match re dir)
			     (not (ido-ignore-item-p dir ido-ignore-directories-merge))
			     (file-directory-p dir))
		    (setq b (substring dir 0 -1)
			  f (concat (file-name-nondirectory b) "/")
			  d (file-name-directory b)
			  res (cons (cons f d) res))))
	      (while dirs
		(setq dir (car dirs)
		      d (car dir)
		      dirs (cdr dirs))
		(when (not (ido-ignore-item-p d ido-ignore-directories-merge))
		  (setq dir (cdr (cdr dir)))
		  (while dir
		    (setq f (car dir)
			  dir (cdr dir))
		    (if (and (string-match re f)
			     (not (ido-ignore-item-p f ido-ignore-directories)))
			(setq res (cons (cons f d) res)))))
		(if (and auto (input-pending-p))
		    (setq dirs nil
			  res t))))))
      (if wide
	  (setq res (ido-wide-find-dirs-or-files
		     ido-current-directory text ido-enable-prefix nil))
	(let ((ido-text text)
	      (dirs ido-work-directory-list)
	      (must-match (and text (> (length text) 0)))
	      dir fl)
	  (if (and auto (not (member ido-current-directory dirs)))
	      (setq dirs (cons ido-current-directory dirs)))
	  (while dirs
	    (setq dir (car dirs)
		  dirs (cdr dirs))
	    (when (and dir (stringp dir)
		       (or ido-merge-ftp-work-directories
			   (not (ido-is-ftp-directory dir)))
		       (file-directory-p dir)
		       (setq fl (if (eq ido-cur-item 'file)
				    (ido-make-file-list1 dir t)
				  (ido-make-dir-list1 dir t))))
	      (if must-match
		  (setq fl (ido-set-matches1 fl)))
	      (if fl
		  (setq res (nconc fl res))))
	    (if (and auto (input-pending-p))
		(setq dirs nil
		      res t))))))
    (if (and res (not (eq res t)))
	(setq res (ido-sort-merged-list res auto)))
    (message nil)
    res))

(defun ido-make-buffer-list1 (&optional frame visible)
  ;; Return list of non-ignored buffer names
  (delq nil 
	(mapcar
	 (lambda (x)
	   (let ((name (buffer-name x)))
	     (if (not (or (ido-ignore-item-p name ido-ignore-buffers) (memq name visible)))
		 name)))
	 (buffer-list frame))))

(defun ido-make-buffer-list (default)
  ;; Return the current list of buffers.
  ;; Currently visible buffers are put at the end of the list.
  ;; The hook `ido-make-buflist-hook' is run after the list has been 
  ;; created to allow the user to further modify the order of the buffer names
  ;; in this list.  If DEFAULT is non-nil, and corresponds to an existing buffer,
  ;; it is put to the start of the list.
  (let* ((ido-current-buffers (ido-get-buffers-in-frames 'current))
	 (ido-temp-list (ido-make-buffer-list1 (selected-frame) ido-current-buffers)))
    (if ido-temp-list
	(nconc ido-temp-list ido-current-buffers)
      (setq ido-temp-list ido-current-buffers))
    (if default
	(progn
	  (setq ido-temp-list 
		(delete default ido-temp-list))
	  (setq ido-temp-list 
		(cons default ido-temp-list))))
    (run-hooks 'ido-make-buffer-list-hook)
    ido-temp-list))

(defun ido-to-end (items)
  ;; Move the elements from ITEMS to the end of `ido-temp-list'
  (mapcar 
   (lambda (elem)  
     (setq ido-temp-list (delq elem ido-temp-list)))
   items)
  (if ido-temp-list
      (nconc ido-temp-list items)
    (setq ido-temp-list items)))

(defun ido-file-name-all-completions (dir)
  ;; Return name of all files in DIR
  ;; Uses and updates ido-dir-file-cache
  (if (and (numberp ido-max-dir-file-cache) (> ido-max-dir-file-cache 0)
	   (stringp dir) (> (length dir) 0)
	   (ido-may-cache-directory dir))
      (let* ((cached (assoc dir ido-dir-file-cache))
	     (ctime (nth 1 cached))   
	     (ftp (ido-is-ftp-directory dir))
	     (attr (if ftp nil (file-attributes dir)))
	     (mtime (nth 5 attr))
	     valid)
	(when cached 	    ; should we use the cached entry ?
	  (if ftp
	      (setq valid (and (eq (car ctime) 'ftp)
			       (ido-cache-ftp-valid (cdr ctime))))
	    (if attr
		(setq valid (and (= (car ctime) (car mtime))
				 (= (car (cdr ctime)) (car (cdr mtime)))))))
	  (if (not valid)
	      (setq ido-dir-file-cache (delq cached ido-dir-file-cache)
		    cached nil)))
	(unless cached
	  (if (and ftp (file-readable-p dir))
	      (setq mtime (cons 'ftp (ido-time-stamp))))
	  (if mtime
	      (setq cached (cons dir (cons mtime (file-name-all-completions "" dir)))
		    ido-dir-file-cache (cons cached ido-dir-file-cache)))
	  (if (> (length ido-dir-file-cache) ido-max-dir-file-cache)
	      (setcdr (nthcdr (1- ido-max-dir-file-cache) ido-dir-file-cache) nil)))
	(and cached
	     (cdr (cdr cached))))
    (file-name-all-completions "" dir)))

(defun ido-make-file-list1 (dir &optional merged)
  ;; Return list of non-ignored files in DIR
  ;; If MERGED is non-nil, each file is cons'ed with DIR
  (and (file-directory-p dir)
       (delq nil 
	     (mapcar
	      (lambda (name)
		(if (not (ido-ignore-item-p name ido-ignore-files t))
		    (if merged (cons name dir) name)))
	      (ido-file-name-all-completions dir)))))

(defun ido-make-file-list (default)
  ;; Return the current list of files.
  ;; Currently visible files are put at the end of the list.
  ;; The hook `ido-make-file-list-hook' is run after the list has been 
  ;; created to allow the user to further modify the order of the file names
  ;; in this list.
  (let ((ido-temp-list (ido-make-file-list1 ido-current-directory)))
    (setq ido-temp-list (ido-sort-list ido-temp-list))
    (let ((default-directory ido-current-directory))
      (ido-to-end ;; move ftp hosts and visited files to end
       (delq nil (mapcar 
		  (lambda (x) (if (or (string-match "..:\\'" x) (get-file-buffer x)) x))
		  ido-temp-list))))
    (ido-to-end  ;; move . files to end
     (delq nil (mapcar 
		(lambda (x) (if (string-equal (substring x 0 1) ".") x))
		ido-temp-list)))
    (if (and default (member default ido-temp-list))
	(if (or ido-rotate-temp ido-rotate-file-list-default)
	    (unless (equal default (car ido-temp-list))
	      (let ((l ido-temp-list) k)
		(while (and l (cdr l) (not (equal default (car (cdr l)))))
		  (setq l (cdr l)))
		(setq k (cdr l))
		(setcdr l nil)
		(nconc k ido-temp-list)
		(setq ido-temp-list k)))
	  (setq ido-temp-list 
		(delete default ido-temp-list))
	  (setq ido-temp-list 
		(cons default ido-temp-list))))
    (when ido-show-dot-for-dired
      (setq ido-temp-list (delete "." ido-temp-list))
      (setq ido-temp-list (cons "." ido-temp-list)))
    (run-hooks 'ido-make-file-list-hook)
    ido-temp-list))

(defun ido-make-dir-list1 (dir &optional merged)
  ;; Return list of non-ignored subdirs in DIR
  ;; If MERGED is non-nil, each subdir is cons'ed with DIR
  (and (file-directory-p dir)
       (delq nil 
	     (mapcar
	      (lambda (name)
		(and (ido-final-slash name) (not (ido-ignore-item-p name ido-ignore-directories))
		     (if merged (cons name dir) name)))
	      (ido-file-name-all-completions dir)))))

(defun ido-make-dir-list (default)
  ;; Return the current list of directories.
  ;; The hook `ido-make-dir-list-hook' is run after the list has been 
  ;; created to allow the user to further modify the order of the
  ;; directory names in this list.
  (let ((ido-temp-list (ido-make-dir-list1 ido-current-directory)))
    (setq ido-temp-list (ido-sort-list ido-temp-list))
    (let ((default-directory ido-current-directory))
      (ido-to-end ;; move visited files to end
       (delq nil (mapcar 
		  (lambda (x) (if (get-file-buffer x) x))
		  ido-temp-list))))
    (ido-to-end  ;; move . files to end
     (delq nil (mapcar 
		(lambda (x) (if (string-equal (substring x 0 1) ".") x))
		ido-temp-list)))
    (if (and default (member default ido-temp-list))
	(if (or ido-rotate-temp ido-rotate-file-list-default)
	    (unless (equal default (car ido-temp-list))
	      (let ((l ido-temp-list) k)
		(while (and l (cdr l) (not (equal default (car (cdr l)))))
		  (setq l (cdr l)))
		(setq k (cdr l))
		(setcdr l nil)
		(nconc k ido-temp-list)
		(setq ido-temp-list k)))
	  (setq ido-temp-list 
		(delete default ido-temp-list))
	  (setq ido-temp-list 
		(cons default ido-temp-list))))
    (setq ido-temp-list (delete "." ido-temp-list))
    (setq ido-temp-list (cons "." ido-temp-list))
    (run-hooks 'ido-make-dir-list-hook)
    ido-temp-list))

(defun ido-get-buffers-in-frames (&optional current)
  ;; Return the list of buffers that are visible in the current frame.
  ;; If optional argument `current' is given, restrict searching to the
  ;; current frame, rather than all frames, regardless of value of
  ;; `ido-all-frames'.
  (let ((ido-bufs-in-frame nil))
    (walk-windows 'ido-get-bufname nil
		  (if current 
		      nil
		    ido-all-frames))
    ido-bufs-in-frame))

(defun ido-get-bufname (win)
  ;; Used by `ido-get-buffers-in-frames' to walk through all windows
  (let ((buf (buffer-name (window-buffer win))))
	(if (not (member buf ido-bufs-in-frame))
	    ;; Only add buf if it is not already in list.
	    ;; This prevents same buf in two different windows being
	    ;; put into the list twice.
	    (setq ido-bufs-in-frame
		  (cons buf ido-bufs-in-frame)))))

;;; FIND MATCHING ITEMS

(defun ido-set-matches1 (items &optional do-full)
  ;; Return list of matches in items
  (let* ((case-fold-search  ido-case-fold)
	 (rexq (if ido-enable-regexp ido-text (regexp-quote ido-text)))
	 (re (if ido-enable-prefix (concat "\\`" rexq) rexq))
	 (full-re (and do-full (not ido-enable-regexp) (not (string-match "\$\\'" re))
		       (concat "\\`" re "\\'")))
	 (prefix-re (and full-re (not ido-enable-prefix)
			 (concat "\\`" rexq)))
	 full-matches
	 prefix-matches
	 matches)
    (mapcar
     (lambda (item)
       (let ((name (ido-name item)))
	 (if (string-match re name)
	     (cond
	      ((and full-re (string-match full-re name))
	       (setq full-matches (cons item full-matches)))
	      ((and prefix-re (string-match prefix-re name))
	       (setq prefix-matches (cons item prefix-matches)))
	      (t (setq matches (cons item matches))))))
       t)
     items)
    (if prefix-matches
	(setq matches (nconc prefix-matches matches)))
    (if full-matches
	(nconc full-matches matches)
      matches)))

(defun ido-set-matches ()
  ;; Set `ido-matches' to the list of items matching prompt
  (when ido-rescan
    (setq ido-matches (ido-set-matches1 (reverse ido-cur-list) (not ido-rotate))
	  ido-rotate nil)))
	 
(defun ido-ignore-item-p (name re-list &optional ignore-ext)
  ;; Return t if the buffer or file NAME should be ignored.
  (and ido-process-ignore-lists re-list
       (let ((data       (match-data))
	     (ext-list   (and ignore-ext ido-ignore-extensions
			      completion-ignored-extensions))
	     ignorep nextstr 
	     (flen (length name)) slen)
	 (while ext-list
	   (setq nextstr (car ext-list))
	   (if (cond
		((stringp nextstr)
		 (and (>= flen (setq slen (length nextstr)))
		      (string-equal (substring name (- flen slen)) nextstr)))
		((fboundp nextstr) (funcall nextstr name))
		(t nil))
	       (setq ignorep t
		     ext-list nil
		     re-list nil)
	     (setq ext-list (cdr ext-list))))
	 (while re-list
	   (setq nextstr (car re-list))
	   (if (cond
		((stringp nextstr) (string-match nextstr name))
		((fboundp nextstr) (funcall nextstr name))
		(t nil))
	       (setq ignorep t
		     re-list nil)
	     (setq re-list (cdr re-list))))
	 ;; return the result
	 (if ignorep
	     (setq ido-ignored-list (cons name ido-ignored-list)))
	 (set-match-data data)
	 ignorep)))

(defun ido-find-common-substring (items subs)
  ;; Return common string following SUBS in each element of ITEMS.
  (let (res
        alist
        ido-change-word-sub)
    (setq ido-change-word-sub
          (if ido-enable-regexp
              subs
            (regexp-quote subs)))
    (setq res (mapcar 'ido-word-matching-substring items))
    (setq res (delq nil res)) ;; remove any nil elements (shouldn't happen)
    (setq alist (mapcar 'ido-makealist res)) ;; could use an  OBARRAY

    ;; try-completion returns t if there is an exact match.
    (let ((completion-ignore-case ido-case-fold))

    (try-completion subs alist))))

(defun ido-word-matching-substring (word)
  ;; Return part of WORD before 1st match to `ido-change-word-sub'.
  ;; If `ido-change-word-sub' cannot be found in WORD, return nil.
  (let ((case-fold-search ido-case-fold)) 
    (let ((m (string-match ido-change-word-sub (ido-name word))))
      (if m
          (substring (ido-name word) m)
        ;; else no match
        nil))))

(defun ido-makealist (res)
  ;; Return dotted pair (RES . 1).
  (cons res 1))

(unless (and (fboundp 'ad-name-p) (ad-name-p 'ido-choose-completion-string))
(defadvice choose-completion-string (around ido-choose-completion-string first activate compile)
  "Adviced for inserting choice in ido-mode minibuffer."
  (if (ido-active)
      (let ((choice (ad-get-arg 0))
	    (buffer (or (ad-get-arg 1) completion-reference-buffer)))
	;; If BUFFER is a minibuffer, barf unless it's the currently active minibuffer.
	(if (and (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer))
		 (or (not (active-minibuffer-window))
		     (not (equal buffer (window-buffer (active-minibuffer-window))))))
	    (error "Minibuffer is not active for completion")
	  ;; Insert the completion into the buffer where completion was requested.
	  (if (get-buffer ido-completion-buffer)
	      (kill-buffer ido-completion-buffer))
	  (cond
	   ((ido-active t) ;; ido-use-merged-list
	    (setq ido-current-directory ""
		  ido-text choice
		  ido-exit 'done))
	   ((not (ido-final-slash choice))
	    (setq ido-text choice
		  ido-exit 'done))
	   (t
	    (ido-set-current-directory ido-current-directory choice)
	    (setq ido-exit 'refresh)))
	  (exit-minibuffer)))
    ad-do-it)))

(defun ido-completion-help ()
  "Show possible completions in a *File Completions* buffer."
  (interactive)
  (setq ido-rescan nil)
  (let ((temp-buf (get-buffer ido-completion-buffer))
	display-it full-list)
    (if (and (eq last-command this-command) temp-buf)
	;; scroll buffer
	(let (win (buf (current-buffer)))
	  (display-buffer temp-buf nil nil)
	  (set-buffer temp-buf)
	  (setq win (get-buffer-window temp-buf))
	  (if (pos-visible-in-window-p (point-max) win)
	      (if (or ido-completion-buffer-all-completions (boundp 'ido-completion-buffer-full))
		  (set-window-start win (point-min))
		(set (make-local-variable 'ido-completion-buffer-full) t)
		(setq full-list t
		      display-it t))
	    (scroll-other-window))
	  (set-buffer buf))
      (setq display-it t))
    (if display-it
	(with-output-to-temp-buffer ido-completion-buffer
	  (let ((completion-list (ido-sort-list
				  (cond
				   (ido-use-merged-list
				    (ido-flatten-merged-list (or ido-matches ido-cur-list)))
				   ((or full-list ido-completion-buffer-all-completions)
				    (ido-all-completions))
				   (t
				    (copy-sequence (or ido-matches ido-cur-list)))))))
	    (if ido-xemacs 
		;; XEmacs extents are put on by default, doesn't seem to be
		;; any way of switching them off.
		(display-completion-list completion-list
					 :help-string "ido "
					 :activate-callback 
					 '(lambda (x y z) (message "doesn't work yet, sorry!")))
	      ;; else running Emacs
	      ;;(add-hook 'completion-setup-hook 'completion-setup-function)
	      (display-completion-list completion-list)))))))

;;; KILL CURRENT BUFFER
(defun ido-kill-buffer-at-head ()
  "Kill the buffer at the head of `ido-matches'."
  (interactive)
  (let ((enable-recursive-minibuffers t)
	(buf (car ido-matches)))
    (when buf
      (kill-buffer buf)
      ;; Check if buffer still exists.
      (if (get-buffer buf)
	  ;; buffer couldn't be killed.
	  (setq ido-rescan t)	
	;; else buffer was killed so remove name from list.
	(setq ido-cur-list (delq buf ido-cur-list))))))

;;; DELETE CURRENT FILE
(defun ido-delete-file-at-head ()
  "Delete the file at the head of `ido-matches'."
  (interactive)
  (let ((enable-recursive-minibuffers t)
	(file (car ido-matches)))
    (if file
	(setq file (concat ido-current-directory file)))
    (when (and file
	       (file-exists-p file)
	       (not (file-directory-p file))
	       (file-writable-p ido-current-directory)
	       (yes-or-no-p (concat "Delete " file " ")))
      (delete-file file)
      ;; Check if file still exists.
      (if (file-exists-p file)
	  ;; file could not be deleted
	  (setq ido-rescan t)	
	;; else file was killed so remove name from list.
	(setq ido-cur-list (delq (car ido-matches) ido-cur-list))))))


;;; VISIT CHOSEN BUFFER
(defun ido-visit-buffer (buffer method &optional record)
  "Visit file named FILE according to METHOD.
Record command in command-history if optional RECORD is non-nil."

  (let (win newframe)
    (cond
     ((eq method 'kill)
      (if record
	  (ido-record-command 'kill-buffer buffer))
      (kill-buffer buffer))

     ((eq method 'samewindow)
      (if record
	  (ido-record-command 'switch-to-buffer buffer))
      (switch-to-buffer buffer))

     ((memq method '(always-frame maybe-frame))
      (cond
       ((and window-system
	     (setq win (ido-window-buffer-p buffer))
	     (or (eq method 'always-frame)
		 (y-or-n-p "Jump to frame? ")))
	(setq newframe (window-frame win))
	(if (fboundp 'select-frame-set-input-focus)
	    (select-frame-set-input-focus newframe)
	  (raise-frame newframe)
	  (select-frame newframe)
	  (if (not ido-xemacs)
	    (set-mouse-position (selected-frame) (1- (frame-width)) 0)))
	(select-window win))
       (t
	;;  No buffer in other frames...
	(if record
	    (ido-record-command 'switch-to-buffer buffer))
	(switch-to-buffer buffer)
	)))

     ((eq method 'otherwindow)
      (if record
	  (ido-record-command 'switch-to-buffer buffer))
      (switch-to-buffer-other-window buffer))

     ((eq method 'display)
      (display-buffer buffer))

     ((eq method 'otherframe)
      (progn
	(switch-to-buffer-other-frame buffer)
	(if (not ido-xemacs)
	    (if (fboundp 'select-frame-set-input-focus)
		(select-frame-set-input-focus (selected-frame))
	      (set-mouse-position (selected-frame) (1- (frame-width)) 0)))
	)))))


(defun ido-window-buffer-p  (buffer)
  ;; Return window pointer if BUFFER is visible in another frame.
  ;; If BUFFER is visible in the current frame, return nil.
  (let ((blist (ido-get-buffers-in-frames 'current)))
    ;;If the buffer is visible in current frame, return nil
    (if (memq buffer blist)
	nil
      ;;  maybe in other frame or icon
      (get-buffer-window buffer 0) ; better than 'visible
      )))


;;; ----------- IDONIZED FUNCTIONS ------------

;;;###autoload
(defun ido-switch-buffer ()
  "Switch to another buffer.
The buffer is displayed according to `ido-default-buffer-method' -- the
default is to show it in the same window, unless it is already visible
in another frame.

As you type in a string, all of the buffers matching the string are
displayed if substring-matching is used \(default). Look at
`ido-enable-prefix' and `ido-toggle-prefix'. When you have found the
buffer you want, it can then be selected. As you type, most keys have their
normal keybindings, except for the following: \\<ido-mode-map>

RET Select the buffer at the front of the list of matches.  If the
list is empty, possibly prompt to create new buffer.

\\[ido-select-text] Select the current prompt as the buffer.
If no buffer is found, prompt for a new one.

\\[ido-next-match] Put the first element at the end of the list.
\\[ido-prev-match] Put the last element at the start of the list.
\\[ido-complete] Complete a common suffix to the current string that 
matches all buffers.  If there is only one match, select that buffer.
If there is no common suffix, show a list of all matching buffers
in a separate window.
\\[ido-edit-input] Edit input string.
\\[ido-fallback-command] Fallback to non-ido version of current command.
\\[ido-toggle-regexp] Toggle regexp searching.
\\[ido-toggle-prefix] Toggle between substring and prefix matching.
\\[ido-toggle-case] Toggle case-sensitive searching of buffer names.
\\[ido-completion-help] Show list of matching buffers in separate window.
\\[ido-enter-find-file] Drop into ido-find-file.
\\[ido-kill-buffer-at-head] Kill buffer at head of buffer list.
\\[ido-toggle-ignore] Toggle ignoring buffers listed in `ido-ignore-buffers'."
  (interactive)
  (ido-buffer-internal ido-default-buffer-method))

;;;###autoload
(defun ido-switch-buffer-other-window ()
  "Switch to another buffer and show it in another window.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'."
  (interactive)
  (ido-buffer-internal 'otherwindow 'switch-to-buffer-other-window))

;;;###autoload
(defun ido-display-buffer ()
  "Display a buffer in another window but don't select it.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'."
  (interactive)
  (ido-buffer-internal 'display 'display-buffer))

;;;###autoload
(defun ido-kill-buffer ()
  "Kill a buffer.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'."
  (interactive)
  (ido-buffer-internal 'kill 'kill-buffer "Kill buffer: " (buffer-name (current-buffer))))

;;;###autoload
(defun ido-insert-buffer ()
  "Insert contents of a buffer in current buffer after point.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'."
  (interactive)
  (ido-buffer-internal 'insert 'insert-buffer "Insert buffer: "))

;;;###autoload
(defun ido-switch-buffer-other-frame ()
  "Switch to another buffer and show it in another frame.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'."
  (interactive)
  (if ido-enabled
      (ido-buffer-internal 'otherframe)
    (call-interactively 'switch-to-buffer-other-frame)))

;;;###autoload
(defun ido-find-file-in-dir (dir)
  "Switch to another file starting from DIR."
  (interactive "DDir: ")
  (if (not (equal (substring dir -1) "/"))
      (setq dir (concat dir "/")))
  (ido-file-internal ido-default-file-method nil dir))

;;;###autoload
(defun ido-find-file ()
  "Edit file with name obtained via minibuffer.
The file is displayed according to `ido-default-file-method' -- the
default is to show it in the same window, unless it is already
visible in another frame.

The file name is selected interactively by typing a substring. As you type
in a string, all of the filenames matching the string are displayed if
substring-matching is used \(default). Look at `ido-enable-prefix' and
`ido-toggle-prefix'. When you have found the filename you want, it can
then be selected. As you type, most keys have their normal keybindings,
except for the following: \\<ido-mode-map>

RET Select the file at the front of the list of matches.  If the
list is empty, possibly prompt to create new file.

\\[ido-select-text] Select the current prompt as the buffer or file.
If no buffer or file is found, prompt for a new one.

\\[ido-next-match] Put the first element at the end of the list.
\\[ido-prev-match] Put the last element at the start of the list.
\\[ido-complete] Complete a common suffix to the current string that 
matches all files.  If there is only one match, select that file.
If there is no common suffix, show a list of all matching files
in a separate window.
\\[ido-edit-input] Edit input string (including path).
\\[ido-prev-work-directory] or \\[ido-next-work-directory] go to previous/next directory in work directory history.
\\[ido-merge-work-directories] search for file in the work directory history.
\\[ido-forget-work-directory] removes current directory from the work directory history.
\\[ido-prev-work-file] or \\[ido-next-work-file] cycle through the work file history.
\\[ido-wide-find-file] and \\[ido-wide-find-dir] prompts and uses find to locate files or directories.
\\[ido-make-directory] prompts for a directory to create in current directory.
\\[ido-fallback-command] Fallback to non-ido version of current command.
\\[ido-toggle-regexp] Toggle regexp searching.
\\[ido-toggle-prefix] Toggle between substring and prefix matching.
\\[ido-toggle-case] Toggle case-sensitive searching of file names.
\\[ido-toggle-vc] Toggle version control for this file.
\\[ido-toggle-literal] Toggle literal reading of this file.
\\[ido-completion-help] Show list of matching files in separate window.
\\[ido-toggle-ignore] Toggle ignoring files listed in `ido-ignore-files'."

  (interactive)
  (ido-file-internal ido-default-file-method))

;;;###autoload
(defun ido-find-file-other-window ()
  "Switch to another file and show it in another window.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (ido-file-internal 'otherwindow 'find-file-other-window))

;;;###autoload
(defun ido-find-alternate-file ()
  "Switch to another file and show it in another window.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (ido-file-internal 'alt-file 'find-alternate-file nil "Find alternate file: "))

;;;###autoload
(defun ido-find-file-read-only ()
  "Edit file read-only with name obtained via minibuffer.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (ido-file-internal 'read-only 'find-file-read-only nil "Find file read-only: "))

;;;###autoload
(defun ido-find-file-read-only-other-window ()
  "Edit file read-only in other window with name obtained via minibuffer.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (ido-file-internal 'read-only 'find-file-read-only-other-window nil "Find file read-only other window: "))

;;;###autoload
(defun ido-find-file-read-only-other-frame ()
  "Edit file read-only in other frame with name obtained via minibuffer.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (ido-file-internal 'read-only 'find-file-read-only-other-frame nil "Find file read-only other frame: "))

;;;###autoload
(defun ido-display-file ()
  "Display a file in another window but don't select it.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (ido-file-internal 'display))

;;;###autoload
(defun ido-find-file-other-frame ()
  "Switch to another file and show it in another frame.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (ido-file-internal 'otherframe 'find-file-other-frame))

;;;###autoload
(defun ido-write-file ()
  "Write current buffer to a file.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (let ((ido-process-ignore-lists t)
	(ido-work-directory-match-only nil)
	(ido-ignore-files (cons "[^/]\\'" ido-ignore-files))
	(ido-report-no-match nil)
	(ido-auto-merge-work-directories-length -1))
    (ido-file-internal 'write 'write-file nil "Write file: ")))

;;;###autoload
(defun ido-insert-file ()
  "Insert contents of file in current buffer.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (ido-file-internal 'insert 'insert-file nil "Insert file: "))

;;;###autoload
(defun ido-dired ()
  "Call dired the ido way.
The directory is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (let ((ido-report-no-match nil)
	(ido-auto-merge-work-directories-length -1))
    (ido-file-internal 'dired 'dired nil "Dired: " 'dir)))

(defun ido-list-directory ()
  "Call list-directory the ido way.
The directory is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (let ((ido-report-no-match nil)
	(ido-auto-merge-work-directories-length -1))
    (ido-file-internal 'list-directory 'list-directory nil "List directory: " 'dir)))

;;; XEmacs hack for showing default buffer

;; The first time we enter the minibuffer, Emacs puts up the default
;; buffer to switch to, but XEmacs doesn't -- presumably there is a
;; subtle difference in the two versions of post-command-hook.  The
;; default is shown for both whenever we delete all of our text
;; though, indicating its just a problem the first time we enter the
;; function.  To solve this, we use another entry hook for emacs to
;; show the default the first time we enter the minibuffer.


;;; ICOMPLETE TYPE CODE

(defun ido-initiate-auto-merge (buffer)
  (ido-trace "\n*merge timeout*" buffer)
  (setq ido-auto-merge-timer nil)
  (when (and (buffer-live-p buffer)
	     (= ido-use-mycompletion-depth (minibuffer-depth))
	     (boundp 'ido-eoinput) ido-eoinput)
    (let ((contents (buffer-substring-no-properties (minibuffer-prompt-end) ido-eoinput)))
      (ido-trace "request merge")
      (setq ido-use-merged-list 'auto
	    ido-text-init contents
	    ido-rotate-temp t
	    ido-exit 'refresh)
      (save-excursion
	(set-buffer buffer)
	(ido-tidy))
      (throw 'ido contents))))

(defun ido-exhibit ()
  "Post command hook for `ido'."
  ;; Find matching files and display a list in the minibuffer.
  ;; Copied from `icomplete-exhibit' with two changes:
  ;; 1. It prints a default file name when there is no text yet entered.
  ;; 2. It calls my completion routine rather than the standard completion.

  (if (= ido-use-mycompletion-depth (minibuffer-depth))
      (let ((contents (buffer-substring-no-properties (minibuffer-prompt-end) (point-max)))
	    (buffer-undo-list t))

	(ido-trace "\nexhibit" this-command)
	(ido-trace "dir" ido-current-directory)
	(ido-trace "contents" contents)
	(ido-trace "list" ido-cur-list)
	(ido-trace "matches" ido-matches)
	(ido-trace "rescan" ido-rescan)

	(save-excursion
	  (goto-char (point-max))
	  ;; Register the end of input, so we know where the extra stuff (match-status info) begins:
	  (if (not (boundp 'ido-eoinput))
	      ;; In case it got wiped out by major mode business:
	      (make-local-variable 'ido-eoinput))
	  (setq ido-eoinput (point))

	  ;; Handle explicit directory changes
	  (and (memq ido-cur-item '(file dir))
	       (> (length contents) 1)
	       (cond
		((ido-final-slash contents)
		 (cond 
		  ((string-equal contents "~/")
		   (ido-set-current-home)
		   t)
		  ((string-equal contents "../")
		   (ido-up-directory t)
		   t)
		  ((string-equal contents "./")
		   t)
		  ((string-match "[$][A-Za-z0-9_]+/\\'" contents)
		   (let ((exp (expand-file-name
			       (substitute-in-file-name (substring contents 0 -1))
			       ido-current-directory)))
		     (if (file-directory-p exp)
			 (ido-set-current-directory exp)
		       (ido-set-current-directory (file-name-directory exp))
		       (setq ido-text-init (file-name-nondirectory exp))))
		   t)
		  ((and (memq system-type '(windows-nt ms-dos))
			(string-equal (substring contents 1) ":/"))
		   (ido-set-current-directory (file-name-directory contents))
		   t)
		  ((string-equal (substring contents -2 -1) "/")
		   (ido-set-current-directory 
		    (if (memq system-type '(windows-nt ms-dos))
			(expand-file-name "/" ido-current-directory)
		      "/"))
		   t)
		  (t 
		   nil)))

		((and (string-equal ido-current-directory "/")
		      (string-match "..:\\'" contents)) ;; Ange-ftp 
		 (ido-set-current-directory "/" contents)
		 (when (ido-is-slow-ftp-host)
		   (setq ido-exit 'fallback)
		   (exit-minibuffer))
		 t)

		((string-equal (substring contents -2 -1) "/")
		 (ido-set-current-directory 
		  (if (= (length contents) 2)
		      "/"
		    (concat ido-current-directory (substring contents 0 -1))))
		 (setq ido-text-init (substring contents -1))
		 t)

		((and (not ido-use-merged-list)
		      (not (ido-final-slash contents))
		      (eq ido-try-merged-list t)
		      (numberp ido-auto-merge-work-directories-length)
		      (> ido-auto-merge-work-directories-length 0)
		      (= (length contents) ido-auto-merge-work-directories-length)
		      (not (input-pending-p)))
		 (setq ido-use-merged-list 'auto
		       ido-text-init contents
		       ido-rotate-temp t)
		 t)

		(t
		 nil))
	       (setq ido-exit 'refresh)
	       (exit-minibuffer))

	  ;; Update the list of matches
	  (setq ido-text contents)
	  (ido-set-matches)
	  (ido-trace "new    " ido-matches)

	  (when (and (not ido-matches)
		   ; ido-rescan
		   ido-process-ignore-lists
		   ido-ignored-list)
	      (let ((ido-process-ignore-lists nil)
		    (ido-rotate ido-rotate)
		    (ido-cur-list ido-ignored-list))
		(ido-trace "try all" ido-ignored-list)
		(ido-set-matches))
	      (when ido-matches
		(ido-trace "found  " ido-matches)
		(setq ido-rescan t)
		(setq ido-process-ignore-lists-inhibit t)
		(setq ido-text-init ido-text)
		(setq ido-exit 'refresh)
		(exit-minibuffer)))

	  (when (and
		 ido-rescan
		 (not ido-matches)
		 (memq ido-cur-item '(file dir))
		 (not (ido-is-root-directory))
		 (> (length contents) 1)
		 (not (string-match "[$]" contents)))
	    (ido-trace "merge?")
	    (if ido-use-merged-list
		(ido-undo-merge-work-directory contents nil)
	      (when (and (eq ido-try-merged-list t)
			 (numberp ido-auto-merge-work-directories-length)
			 (= ido-auto-merge-work-directories-length 0)
			 (not (input-pending-p)))
		(ido-trace "\n*start timer*")
		(setq ido-auto-merge-timer
		      (run-with-timer ido-auto-merge-delay-time nil 'ido-initiate-auto-merge (current-buffer))))))
	  
	  (setq ido-rescan t)

	  (if (and ido-use-merged-list 
		   ido-matches
		   (not (string-equal (car (cdr (car ido-matches))) ido-current-directory)))
	      (progn
		(ido-set-current-directory (car (cdr (car ido-matches))))
		(setq ido-use-merged-list t
		      ido-exit 'keep
		      ido-text-init ido-text)
		(exit-minibuffer)))

	  ;; Insert the match-status information:
	  (ido-set-common-completion)
	  (let ((inf (ido-completions 
		     contents
		     minibuffer-completion-table
		     minibuffer-completion-predicate
		     (not minibuffer-completion-confirm))))
	    (ido-trace "inf" inf)
	    (insert inf))
	  
	  ))))

(defun ido-completions (name candidates predicate require-match)
  ;; Return the string that is displayed after the user's text.
  ;; Modified from `icomplete-completions'.
  
  (let* ((comps ido-matches)
	 (ind (and (consp (car comps)) (> (length (cdr (car comps))) 1)
		   ido-merged-indicator))
	 first)

    (if (and ind ido-use-faces)
	(put-text-property 0 1 'face 'ido-indicator-face ind))
	
    (if (and ido-use-faces comps)
	(let* ((fn (ido-name (car comps)))
	       (ln (length fn)))
	  (setq first (format "%s" fn))
	  (put-text-property 0 ln 'face
			     (if (= (length comps) 1)
				 'ido-only-match-face
			       'ido-first-match-face)
			     first)
	  (if ind (setq first (concat first ind)))
	  (setq comps (cons first (cdr comps)))))

    (cond ((null comps)
	   (if ido-report-no-match
	       (nth 6 ido-decorations)  ;; [No Match]
	     ""))

	  ((null (cdr comps))		;one match
	   (concat (if (> (length (ido-name (car comps))) (length name))
		       ;; when there is one match, show the matching file name in full
		       (concat (nth 4 ido-decorations)  ;; [ ... ]
			       (ido-name (car comps))
			       (nth 5 ido-decorations))
		     "")
		   (if (not ido-use-faces) (nth 7 ido-decorations))))  ;; [Matched]
	  (t				;multiple matches
	   (let* ((items (if (> ido-max-prospects 0) (1+ ido-max-prospects) 999))
		  (alternatives
		   (apply
		    (function concat)
		    (cdr (apply
			  (function nconc)
			  (mapcar '(lambda (com)
				     (setq com (ido-name com))
				     (setq items (1- items))
				     (cond
				      ((< items 0) ())
				      ((= items 0) (list (nth 3 ido-decorations))) ; " | ..."
				      (t
				       (list (or ido-separator (nth 2 ido-decorations)) ; " | "
					     (let ((str (substring com 0)))
					       (if (and ido-use-faces
							(not (string= str first))
							(ido-final-slash str))
						   (put-text-property 0 (length str) 'face 'ido-subdir-face str))
					       str)))))
				  comps))))))

	     (concat
	      ;; put in common completion item -- what you get by pressing tab
	      (if (> (length ido-common-match-string) (length name))
		  (concat (nth 4 ido-decorations)   ;; [ ... ]
			  (substring ido-common-match-string (length name))
			  (nth 5 ido-decorations)))
	      ;; list all alternatives
	      (nth 0 ido-decorations)  ;; { ... }
	      alternatives
	      (nth 1 ido-decorations)))))))

(defun ido-minibuffer-setup ()
  "Minibuffer setup hook for `ido'."
  ;; Copied from `icomplete-minibuffer-setup-hook'.
  (when (and (boundp 'ido-completing-read) 
	     (or ido-xemacs (= ido-use-mycompletion-depth (minibuffer-depth))))
    (make-local-hook 'pre-command-hook)
    (add-hook 'pre-command-hook 'ido-tidy nil t)
    (make-local-hook 'post-command-hook)
    (add-hook 'post-command-hook 'ido-exhibit nil t)
    (when ido-xemacs
      (ido-exhibit)
      (goto-char (point-min)))
    (run-hooks 'ido-minibuffer-setup-hook)))

(defun ido-tidy ()
  "Pre command hook for `ido'."
  ;; Remove completions display, if any, prior to new user input.
  ;; Copied from `icomplete-tidy'."

  (when ido-auto-merge-timer
    (ido-trace "\n*cancel timer*" this-command)
    (cancel-timer ido-auto-merge-timer)
    (setq ido-auto-merge-timer nil))

  (if (and (boundp 'ido-use-mycompletion-depth)
	   (= ido-use-mycompletion-depth (minibuffer-depth)))
      (if (and (boundp 'ido-eoinput)
	       ido-eoinput)
      
	  (if (> ido-eoinput (point-max))
	      ;; Oops, got rug pulled out from under us - reinit:
	      (setq ido-eoinput (point-max))
	    (let ((buffer-undo-list t))
	      (delete-region ido-eoinput (point-max))))
    
	;; Reestablish the local variable 'cause minibuffer-setup is weird:
	(make-local-variable 'ido-eoinput)
	(setq ido-eoinput 1))))

(defun ido-summary-buffers-to-end ()
  ;; Move the summaries to the end of the buffer list.
  ;; This is an example function which can be hooked on to
  ;; `ido-make-buffer-list-hook'.  Any buffer matching the regexps
  ;; `Summary' or `output\*$'are put to the end of the list.
  (let ((summaries (delq nil (mapcar 
			      (lambda (x) 
				 (if (or 
				      (string-match "Summary" x)
				      (string-match "output\\*\\'" x))
				     x))
			      ido-temp-list))))
    (ido-to-end summaries)))

;;; Helper functions for other programs

;;;###autoload
(defun ido-read-file-name (prompt &optional dir default-filename mustmatch initial)
  "Read file name, prompting with PROMPT and completing in directory DIR.
See `read-file-name' for additional parameters."
  (let (filename
	ido-saved-vc-mt
	(vc-master-templates (and (boundp 'vc-master-templates) vc-master-templates))
	(ido-current-directory (expand-file-name (or dir default-directory)))
	(ido-work-directory-index -1)
	(ido-work-file-index -1)
	(ido-find-literal nil))
    (setq filename
	  (ido-read-internal 'file prompt 'ido-file-history default-filename mustmatch initial))
    (if filename
	(concat ido-current-directory filename))))

;;;###autoload
(defun ido-read-directory-name (prompt &optional dir default-dirname mustmatch initial)
  "Read directory name, prompting with PROMPT and completing in directory DIR.
See `read-file-name' for additional parameters."
  (let (filename
	ido-saved-vc-mt
	(ido-current-directory (expand-file-name (or dir default-directory)))
	(ido-work-directory-index -1)
	(ido-work-file-index -1))
    (setq filename
	  (ido-read-internal 'dir prompt 'ido-file-history default-dirname mustmatch initial))
    (if filename
	(if (and (stringp filename) (string-equal filename "."))
	    ido-current-directory
	  (concat ido-current-directory filename)))))

;;; ido.el
