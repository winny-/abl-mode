;; Helpers

(defun comment ()
  nil)


(defconst abl-keywords
       '("def" "define" "var" "variable" "char" "character" "int" "integer" "dec" "decimal"
	     "log" "logical" "as" "extent" "if" "then" "else" "end" "do" "elseif" "endif"
	     "message" "date" "absolute" "and" "or" "assign" "available" "beings" "recid"
	     "can-do" "can-find" "case" "when" "create" "day" "month" "year" "datetime"
	     "procedure" "function" "forward" "returns" "temp-table" "for" "each"
	     "delete" "in" "empty" "find" "handle" "first" "last" "length" "modulo" "not"
	     "now" "today" "output" "stream" "index" "rindex" "replace" "round" "string"
	     "rowid" "sqrt" "substring" "trim" "tran" "undo" "leave" "input" "output"
		 "return" "num-entries" "subst" "no-undo" "disp" "format" "with" "down" "frame"
	     "find first"
	     "group by"))

;; Define Mode
(defvar abl-mode-hook nil)

(defvar abl-mode-map
  (let ((map (make-keymap)))
    ;;Define mode-specific keybindings here
    map))

(add-to-list 'auto-mode-alist '("\\.p\\'" . abl-mode))


;;Highlighting
(defvar keyword-re
  (concat "\\<"
		  (regexp-opt ;;build me a regex worthy of mordor that matches all of these!
		   (append
			(mapcar 'upcase abl-keywords)
			'("{\.+}"))
		   t)
		  "\\>"))

(defconst abl-font-lock-keywords-1
  (list
   '(keyword-re . font-lock-builtin-face)))

(defvar abl-mode-syntax-table
  (let ((abl-mode-syntax-table (make-syntax-table)))
	(modify-syntax-entry ?- "w" abl-mode-syntax-table)
	(modify-syntax-entry ?/ ". 24" abl-mode-syntax-table)
	(modify-syntax-entry ?* ". 23" abl-mode-syntax-table)
	abl-mode-syntax-table))
						 






;;Indentation
(defun look-prev (expr)
  (save-excursion
	(progn
	  (forward-line -1)
	  (if (looking-at "^[ \\t]*$")
		  (look-prev expr)
		(looking-at expr)))))

(defun look-pprev (expr)
  (save-excursion
	(progn
	  (forward-line -1)
	  (if (looking-at "^[ \\t]*$")
		  (look-pprev expr)
		(look-prev expr)))))

(defun ind-of-prev ()
  (save-excursion
	(progn
	  (forward-line -1)
	  (current-indentation))))

(defun abl-indent-line ()
  "Indent currnet line as ABL."
  (interactive)
  (beginning-of-line)
  (let (cur-indent)
	(cond
	 ((bobp) ;don't indent first line of file
	  (indent-line-to 0))

	 ((look-prev ".*\\.[ \\t]*$") ;prev ends .
	  (if (or
		   (looking-at "^[\\t ]*END\\.[\\t ]*$") ;end of block
		   (look-pprev "[^\\.][\\t ]*$")) ;out of multi-line
		  (setq cur-indent (- (ind-of-prev) default-tab-width))
		(setq cur-indent (ind-of-prev))))
	 ((look-prev ".*[^\\.][\\t ]*$") ;prev doesn't end .
	  (if (look-pprev ".*\\.[ \\t]*$")
		  (setq cur-indent (+ (ind-of-prev) default-tab-width))
		(setq cur-indent (ind-of-prev)))))
	(if cur-indent
		(if (> cur-indent 0)
			(indent-line-to cur-indent)
		  (indent-line-to 0))
	  (indent-line-to 0))))
		  
	 
;; Capitalization
(define-abbrev-table 'fundamental-mode-abbrev-table
  (mapcar #'(lambda (v) (list v (upcase v) nil 1))
	  abl-keywords))


;; Misc
(defun misc ()
  (setq abbrev-mode t)
  (setq save-abbrevs nil)
  (setq indent-tabs-mode nil)
  (setq tab-stop-list (number-sequence 0 200 4)))


;; Synthesis
(defun abl-mode ()
  "Major mode for editing ABL files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table abl-mode-syntax-table)
  (use-local-map abl-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(abl-font-lock-keywords-1))
  (set (make-local-variable 'indent-line-function) 'abl-indent-line)
  (setq major-mode 'abl-mode)
  (setq mode-name "ABL")
  (misc)
  (run-hooks 'abl-mode-hook))

(provide 'abl-mode)


;; ===TODO:===
;;
;;  * indentation for assign statements and param lists
;;  * syntax highlighting
