		       SES - Simple Emacs Spreadsheet

SES is a major mode for GNU Emacs to edit spreadsheet files, which contain a
rectangular grid of cells.  The cells' values are specified by formulas that
can refer to the values of other cells.

This document describes SES-21 rev 031130, for GNU Emacs 21.1.  If you are
using GNU Emacs 20.3, please see http://home.comcast.net/~jyavner/ses20

This documentation is part of the Emacs info tree and is distributed under
the GNU Free Documentation License.  (C) 2002 Free Software Foundation, Inc.


+-------------+
| SALES PITCH |
+-------------+

* Create and edit simple spreadsheets with a minimum of fuss.
* Full undo/redo/autosave.
* Immune to viruses in spreadsheet files.
* Cell formulas are straight Emacs Lisp.
* Printer functions for control of cell appearance.
* Intuitive keystroke commands: C-o = insert row, M-o = insert column, etc.
* "Spillover" of lengthy cell values into following blank cells.
* Header line shows column letters or a selected row.
* Completing-read for entering symbols as cell values.
* Cut, copy, and paste can transfer formulas and printer functions.
* Import and export of tab-separated values or tab-separated formulas.
* Plaintext, easily-hacked file format.

* NEW FOR 21.1: Viral immunity, header line, sort by column, customizable
  variables.  Also, great speed improvements (from deferred updates) and
  some bugfixes.


+--------------+
| INSTALLATION |
+--------------+

To make SES available for use, put these lines in your .emacs file:
   (add-to-list 'load-path "/ses-path/")
   (autoload 'ses-mode "ses.el" "Spreadsheet mode" t)
where "/ses-path/" is the filesystem path to your copies of "ses.elc" and
"unsafep.elc".

To create new spreadsheets, either use `M-x ses-mode' on an empty buffer or
put this in your .emacs file:
  (add-to-list 'auto-mode-alist '("\\.ses$" . ses-mode))


+------------+
| THE BASICS |
+------------+

* A cell identifier is a symbol with a column letter and a row number.
  Cell B7 is the 2nd column of the 7th row.  For very wide spreadsheets,
  there are two column letters: cell AB7 is the 28th column of the 7th row.
    `j'       (ses-jump)        - Moves point to cell, specified by identifier.

* Point is always at the left edge of a cell, or at the empty endline.  When
  mark is inactive, the current cell is underlined.   When mark is active, the
  range is the highlighted rectangle of cells (SES always uses transient mark
  mode).  Drag the mouse from A1 to A3 to create the range A1-A2.  Many
  SES commands operate only on single cells, not ranges.
    `M-h'     (ses-mark-row)     - Highlight current row.
    `S-M-h'   (ses-mark-column)  - Highlight current column.
    `C-x h'   (mark-whole-buffer)- Highlight all cells.
    `C-SPC'   (set-mark-command) - Set mark at point.
    `C-@'     (set-mark-command) - Some keyboards can't make C-SPC.
    `C-g'     (keyboard-quit)    - Turn off the mark.


Entering a formula:

* To enter a number into the current cell, just start typing:
    `0'...`9' (ses-read-cell) - Self-insert a digit.
    `-'       (ses-read-cell) - Self-insert a negative number.
    `.'       (ses-read-cell) - Self-insert a fraction.

* To enter a string, begin with a double-quote.  The ending double-quote is
  inserted for you:
    `"'       (ses-read-cell) - Self-insert a quoted string.

* To enter a Lisp expression, begin with a left-parenthesis.  The
  right-parenthesis is inserted for you:
    `('       (ses-read-cell) - Self-insert an expression.
  To access another cell's value, just use its identifier in your expression.
  Whenever the other cell is changed, this cell's formula will be reevaluated.
  While typing in the expression, you can use M-TAB to complete symbol names.

* To enter a symbol, begin with an apostrophe.
    '         (ses-read-symbol)
  SES remembers all symbols that have been used as formulas, so you can type
  just the beginning of a symbol and use SPC, TAB, and ? to complete it.

* To enter something else (e.g., a vector), begin with a digit, then erase the
  digit and type whatever you want.

* To edit the existing formula in the current cell, press the ENTER key:
    `RET'     (ses-edit-cell)

* To force reevaluation of formulas:
    `C-c C-c' (ses-recalculate-cell) - Recalculate current cell or range.
    `C-c C-l' (ses-recalculate-all)


Resizing the spreadsheet:

* Basic commands:
    `C-o'     (ses-insert-row)
    `M-o'     (ses-insert-column)
    `C-k'     (ses-delete-row)
    `M-k'     (ses-delete-column)
    `w'       (ses-set-column-width)

* TAB moves point to the next rightward cell, or inserts a new column if
  already at last cell on line, or inserts a new row if at endline.
    `TAB'     (ses-forward-or-insert)

* Linefeed inserts below the current row and moves to column A:
    `C-j'     (ses-append-row-jump-first-column)

* Resizing the spreadsheet (unless you're just changing a column width)
  relocates all the cell-references in formulas so they still refer to the
  same cells.  If a formula mentioned B1 and you insert a new first row, the
  formula will now mention B2.

* If you delete a cell that a formula refers to, the cell-symbol is deleted
  from the formula, so (+ A1 B1 C1) after deleting the third column becomes
  (+ A1 B1).  In case this is not what you wanted:
    `C-_'      (undo)
    `C-x u'    (undo) - Some keyboards have trouble with C-_.


Entering a printer function:

* Printer functions convert binary cell values into the print forms that
  Emacs will display on the screen.

* A printer can be a format string, like "$%.2f".  The result string is
  right-aligned within the print cell.  To get left-alignment, use
  parentheses: ("$%.2f").  A printer can also be a one-argument function (a
  symbol or a lambda), whose result is a string (right-aligned) or list of one
  string (left-aligned).  While typing in a lambda, you can use M-TAB to
  complete the names of symbols.

* Each cell has a printer.  If nil, the column-printer for the cell's column
  is used.  If that is also nil, the default-printer for the spreadsheet is
  used.
    `p'       (ses-read-cell-printer) -- Can be used with a range
    `M-p'     (ses-read-column-printer)
    `C-c C-p' (ses-read-default-printer)

* The ses-read-XXX-printer commands have their own minibuffer history, which
  is preloaded with the set of all printers used in this spreadsheet, plus
  the standard printers.

* The standard printers are suitable only for cells, not columns or default,
  because they format the value using the column-printer (or default-printer
  if nil) and then center the result:
    `ses-center'          Just centering
    `ses-center-span'     Centering with spill-over to following blank cells
    `ses-dashfill'        Centering using dashes (-) instead of spaces
    `ses-dashfill-span'   Centering with dashes and spill-over
    `ses-tildefill-span'  Centering with tildes (~) and spill-over


Clearing cells:

* These commands set both formula and printer to nil:
    `DEL'     (ses-clear-cell-backward) - Clear cell and move left.
    `C-d'     (ses-clear-cell-forward)  - Clear cell and move right.


Copy, cut, and paste:

* The copy functions work on rectangular regions of cells.  You can paste the
  copies into non-SES buffers to export the print text.
    `M-w'            (kill-ring-save)
    [copy]           (kill-ring-save) - if your keyboard has this.
    [C-insert]       (kill-ring-save) - if your keyboard has this.
    [drag-mouse-1]   (mouse-set-region)
    [M-drag-mouse-1] (mouse-set-secondary)

* The cut functions do not actually delete rows or columns - they copy and
  then clear:
    `C-w'            (ses-kill-override)
    [cut]            (ses-kill-override) - if your keyboard has this.
    [S-delete]       (ses-kill-override) - if your keyboard has this.

* The paste functions behave differently depending on the format of the text
  being inserted.  When pasting cells that were cut from a SES buffer, the
  print text is ignored and only the attached formula and printer are
  inserted; cell references in the formula are relocated unless you use C-u.
  Generally, non-SES text is inserted as a replacement formula for the current
  cell.  If the formula would be a symbol, it's treated as a string unless you
  use C-u.  Pasted formulas with syntax errors are always treated as strings.
    `C-y'            (yank)
    [S-insert]       (yank)	      - if your keyboard has this.
    [paste]          (clipboard-yank) - if your keyboard has this.
    [mouse-2]        (mouse-yank-at-click)
    [M-mouse-2]      (mouse-yank-secondary)
  The pasted text overwrites a rectangle of cells whose top left corner is the
  current cell.  If part of the rectangle is beyond the edges of the
  spreadsheet, you must confirm the increase in spreadsheet size.

* Immediately after a paste, you can replace the text with a preceding
  element from the kill ring.
    `M-y'            (ses-yank-pop)
  Unlike the standard Emacs `yank-pop', the SES version uses `undo' to delete
  the old yank.  This doesn't make any difference?


Customizing SES:

* By default, a newly-creating spreadsheet has 1 row and 1 column.  The
  column width is 7 and the default printer is "%.7g".  Each of these can be
  customized.  Look in group "ses".

* After entering a cell value, point normally moves right to the next cell.
  You can customize `ses-after-entry-functions' to move left or up or down.
  For diagonal movement, select two functions from the list.

* `ses-mode-hook' is a normal mode hook (list of functions to execute when
  starting SES mode for a buffer).

* The variable `safe-functions' is a a list of possibly-unsafe functions to
  be treated as safe when analysing formulas and printers.  See "Virus
  protection" below.  Before customizing `safe-functions', think about how
  much you trust the person suggesting this change.  The value t turns off
  all anti-virus protection.  A list-of-functions value might enable a
  gee-whiz spreadsheet, but it also creates trapdoors in your anti-virus
  armor.  In order for virus protection to work, you must always press N
  when presented with a virus warning, unless you understand what the
  questionable code is trying to do.  Do not listen to those who tell you to
  customize `enable-local-eval' -- this variable is for people who don't
  wear safety belts!


+-------------------+
| ADVANCED FEATURES |
+-------------------+

* A SES file consists of a print area and a data area.  Normally the buffer is
  narrowed to show only the print area.  The data area records the formula
  and printer functions, etc.  If you want to see it:
    `C-x n w' (widen)               - Shows the print and data areas.
    `C-c C-n' (ses-renarrow-buffer) - Shows only the print area.

* The print area is read-only except for special SES commands.  It contains
  cell values formatted by printer functions.  If it gets messed up:
    `S-C-l'   (ses-reprint-all)
    `M-C-l'   (ses-reprint-all) - Some keyboards can't make S-C-l.

* The header line at the top of the SES window normally shows the column
  letter for each column.  You can set it to show a copy of some row, such
  as a row of column titles, so that row will always be visible.  Set the
  header line to row 0 to show column letters again.
    `C-c M-C-h'      (ses-read-header-row)


Compatibility with older SES revs:

* SES file-format 2 includes support for a header-line row, while SES
  file-format 1 doesn't.  If you create a spreadsheet using the version of
  SES that runs on Emacs 20.2 and then edit it using this version, it will
  automatically be upgraded to file-format 2 when saved.

* If you need to convert back to file-format 1 to maintain compatibility
  with Emacs 20.2, you have to manually edit the data area:  Delete the
  "ses-header-row" line and change the number before ";SES file-format" to 1.
  After making these changes, save the file and kill the buffer -- bad
  things can happen if you use SES commands on a buffer that's been
  converted back to file-format 1.


Ranges in formulas:

* A formula like (+ A1 A2 A3) is the sum of three specific cells.  If you
  insert a new second row, the formula becomes (+ A1 A3 A4) -- the new row is
  not included in the sum.

* The macro (ses-range FROM TO) evalutes to a list of the values in a
  rectangle of cells.  If your formula is (apply '+ (ses-range A1 A3)) and you
  insert a new second row, it becomes (apply '+ (ses-range A1 A4)) and the new
  row is included in the sum.

* While entering or editing a formula in the minibuffer, you can select a
  range in the spreadsheet (using mouse or keyboard), then paste a
  representation of that range into your formula.  Suppose you select A1-C1.
    [S-mouse-3]   (ses-insert-range-click)     - Inserts "A1 B1 C1"
    `C-c C-r'     (ses-insert-range)           - Keyboard version
    [C-S-mouse-3] (ses-insert-ses-range-click) - Inserts "(ses-range A1 C1)"
    `C-c C-s'     (ses-insert-ses-range)       - Keyboard version

* If you delete the FROM or TO cell for a range, the nearest still-existing
  cell is used instead.  If you delete the entire range, the formula relocator
  will delete the ses-range from the formula.

* If you insert a new row just beyond the end of a one-column range, or a new
  column just beyond a one-row range, the new cell is included in the range.
  New cells inserted just before a range are not included.


Sorting by column:

* You can sort the cells of a range using one of the columns.  The rows (or
  partial rows if the range doesn't include all columns) are rearranged so
  the selected column will be in order.  Easiest way to sort is to click
  mouse-2 on the column's header row.  With prefix arg, sort is in
  descending order.
    `C-c M-C-s'           (ses-sort-column)
    [header-line mouse-2] (ses-sort-column-click)

* The sort comparison uses `string<', which works well for right-justified
  numbers and left-justified strings.

* Rows are moved one at a time, with relocation of formulas.  This works
  well if formulas refer to other cells in their row, not so well for
  formulas that refer to other rows in the range or to cells outside the
  range.


Special cell values:

* nil prints the same as "", but allows previous cell to spill over.

* '*skip* replaces nil when the previous cell actually does spill over;
  nothing is printed for it.

* '*error* indicates that the formula signalled an error instead of producing
  a value: the print cell is filled with hash marks (#).


Standard formula functions:

* Oftentimes you want a calculation to exclude the blank cells.  Here are some
  useful functions to call from your formulas:

* (ses-delete-blanks &rest ARGS)
  Returns a list from which all blank cells (value is either nil or '*skip*)
  have been deleted.

* (ses+ &rest ARGS)
  Sum of non-blank arguments.

* (ses-average LIST)
  Average of non-blank elements in LIST.  Here the list is passed as a single
  argument, since you'll probably use it with ses-range.


More on cell printing:

* If the result from the printer function is too wide for the cell and the
  following cell is nil, the result will spill over into the following cell.
  Very wide results can spill over several cells.  If the result is too wide
  for the available space (up to the end of the row or the next non-nil
  cell), the result is truncated if the cell's value is a string, or
  replaced with hash marks otherwise.

* You cannot move point to a cell that has been covered by a spill-over.  Use
    `C-c C-t'  (ses-truncate-cell)
  to confine a cell to its own column so you can put something in a rightward
  column.  If you don't change the rightward column, the cell will spill over
  again the next time it is reprinted.

* SES could get confused by printer results that contain newlines or tabs, so
  these are replaced with question marks.

* If a cell's printer signals an error, the default printer "%s" is used
  instead.  This is good if your column printer is numeric-only and you use a
  string as a cell value.  When it's bad, find out what's happening by using
    `C-c C-c' (ses-recalculate-cell)
  When used on a single cell, C-c C-c will display in the echo area any
  formula error or printer error that occurred during recalculation/reprinting.


Import and export:

* To export a range of cells:
    `x t'      (ses-export-tsv) - Tab-separated values
    `x T'      (ses-export-tsf) - Tab-separated formulas

* The exported text goes to the kill ring - you can paste it into another
  buffer.  Columns are separated by tabs, rows by newlines.

* To import text, use any of the yank commands where the text to paste
  contains tabs and/or newlines.  Imported formulas are not relocated.


Virus protection:

* Whenever a formula or printer is read from a file or is pasted into the
  spreadsheet, it receives a "needs safety check" marking.  Later, when the
  formula or printer is evaluated for the first time, it is checked for
  safety using the `unsafep' predicate; if found to be "possibly unsafe",
  the questionable formula or printer is displayed and you must press Y to
  approve it or N to use a substitute.  The substitute always signals an
  error.

* Formulas or printers that you type in are checked immediately for safety.
  If found to be possibly unsafe and you press N to disapprove, the action
  is cancelled and the old formula or printer will remain.

* Besides viruses (which try to copy themselves to other files), unsafep
  can also detect all other kinds of Trojan horses, such as
  spreadsheets that delete files, send email, flood Web sites, alter your
  Emacs settings, etc.

* Generally, spreadsheet formulas and printers are simple things that don't
  need to do any fancy computing, so all potentially-dangerous parts of the
  Emacs Lisp environment can be excluded without cramping your style as a
  formula-writer.  See the documentation in unsafep.el for more info on how
  Lisp forms are classified as safe or unsafe.


Spreadsheets with details and summary:

* A common organization for spreadsheets is to have a bunch of "detail" rows,
  each perhaps describing a transaction, and then a set of "summary" rows that
  each show reduced data for some subset of the details.  SES supports this
  organization via the `ses-select' function.

* (ses-select FROMRANGE TEST TORANGE)
  Returns a subset of TORANGE.  For each member in FROMRANGE that is equal to
  TEST, the corresponding member of TORANGE is included in the result.

* Example of use:
    (ses-average (ses-select (ses-range A1 A5) 'Smith (ses-range B1 B5)))
  This computes the average of the B column values for those rows whose A
  column value is the symbol 'Smith.

* Arguably one could specify only FROMRANGE plus TO-ROW-OFFSET and
  TO-COLUMN-OFFSET.  The TORANGE is stated explicitly to ensure that the
  formula will be recalculated if any cell in either range is changed.

* See file "ses-example.el" for an example of a details-and-summary
  spreadsheet.


+-----------+
| FOR GURUS |
+-----------+

Progress messages:

* Generally, a time-consuming SES operation will display a progress message
  very second or so.  The variable `ses-progress-frequency' (from SES rev
  011128 for GNU Emacs 20.2) is no longer used.


Deferred updates:

* To save time by avoiding redundant computations, cells that need
  recalculation due to changes in other cells are added to a set.  At the
  end of the command, each cell in the set is recalculated once.  This can
  create a new set of cells that need recalculation.  The process is
  repeated until either the set is empty or it stops changing (due to
  circular references among the cells).  In extreme cases, you might see
  progress messages of the form "Recalculating... (nnn cells left)".  If you
  interrupt the calculation using C-g, the spreadsheet will be left in an
  inconsistent state, so use C-_ or C-c C-l to fix it.

* To save even more time by avoiding redundant writes, cells that have
  changes are added to a set instead of being written immediately to the
  data area.  Each cell in the set is written once, at the end of the
  command.  If you change vast quantities of cells, you might see a progress
  message of the form "Writing... (nnn cells left)".  These deferred
  cell-writes cannot be interrupted by C-g, so you'll just have to wait.

* SES uses `run-with-idle-timer' to move the cell underline when Emacs will
  be scrolling the buffer after the end of a command, and also to narrow and
  underline after C-x C-v.  This is visible as a momentary glitch after
  certain C-x C-v and certain scrolling commands.  You can type ahead
  without worrying about the glitch.


Nonrelocatable references:

* `C-y' relocates all cell-references in a pasted formula, while `C-u C-y'
  relocates none of the cell-references.  What about mixed cases?

* You can use (symbol-value 'B3) to make an "absolute reference".  The formula
  relocator skips over quoted things, so this will not be relocated when
  pasted or when rows/columns are inserted/deleted.  However, B3 will not be
  recorded as a dependency of this cell, so this cell will not be updated
  automatically when B3 is changed.

* The variables `row' and `col' are dynamically bound while a cell formula is
  being evaluated.  You can use (ses-cell-value row 0) to get the value from
  the leftmost column in the current row.  This kind of dependency is also not
  recorded.


The data area:

* Begins with an 014 character, followed by sets of cell-definition macros
  for each row, followed by column-widths,  column-printers, default-printer,
  and header-row.  Then there's the global parameters (file-format ID,
  numrows, numcols) and the local variables (specifying SES mode for the
  buffer, etc.)

* When a SES file is loaded, first the numrows and numcols values are loaded,
  then the entire data area is eval'ed, and finally the local variables are
  processed.

* You can edit the data area, but don't insert or delete any newlines except
  in the local-variables part, since SES locates things by counting newlines.
  Use C-x C-e at the end of a line to install your edits into the spreadsheet
  data structures (this does not update the print area, use e.g. C-c C-l for
  that).

* The data area is maintained as an image of spreadsheet data structures that
  area stored in buffer-local variables.  If the data area gets messed up, you
  can try reconstructing the data area from the data structures:
    `C-c M-C-l'    (ses-reconstruct-all)


Buffer-local variables in spreadsheets:

* You can add additional local variables to the list at the bottom of the data
  area, such as hidden constants you want to refer to in your formulas.

* You can override the variable `symbolic-formulas' to be a list of symbols
  (as parenthesized strings) to show as completions for the ' command.  This
  initial completions list is used instead of the actual set of
  symbols-as-formulas in the spreadsheet.

* For examples of these, see "ses-example.ses".

* If (for some reason) you want your formulas or printers to save data into
  variables, you must declare these variables as buffer-locals in order to
  avoid a virus warning.

* You can define functions by making them values for the fake local variable
  'eval'.  Such functions can then be used in your formulas and printers, but
  usually each 'eval' is presented to the user during file loading as a
  potential virus - this can get annoying.

* You can define functions in your .emacs file.  Other people can still
  read the print area of your spreadsheet, but they won't be able to
  recalculate or reprint anything that depends on your functions.  To avoid
  virus warnings, each function used in a formula needs
      (put 'your-function-name 'safe-function t)


Uses of defadvice in SES:

* undo-more: Defines a new undo element format (FUN . ARGS), which means "undo
  by applying FUN to ARGS".  For spreadsheet buffers, it allows undos in the
  data area even though that's outside the narrowing.

* copy-region-as-kill: When copying from the print area of a spreadsheet,
  treat the region as a rectangle and attach each cell's formula and printer
  as 'ses properties.

* yank: When yanking into the print area of a spreadsheet, first try to yank
  as cells (if the yank text has 'ses properties), then as tab-separated
  formulas, then (if all else fails) as a single formula for the current cell.
