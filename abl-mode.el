
;; Helpers

(defun comment ()
  nil)


;; see also: https://decada.googlecode.com/hg/Editra/src/syntax/_progress.py
(defconst abl-keywords
       '("def" "define" "var" "variable" "char" "character" "int" "integer" "dec" "decimal"
	     "log" "logical" "as" "extent" "if" "then" "else" "end" "do" "elseif" "endif"
	     "message" "date" "absolute" "and" "or" "assign" "available" "beings" "recid"
	     "can-do" "can-find" "case" "when" "create" "day" "month" "year" "datetime"
	     "procedure" "function" "forward" "returns" "temp-table" "for" "each"
	     "delete" "in" "empty" "find" "handle" "first" "last" "length" "modulo" "not"
	     "now" "today" "output" "stream" "index" "rindex" "replace" "round" "string"
	     "rowid" "sqrt" "substring" "trim" "tran" "leave" "input" "output"
		 "return" "num-entries" "subst" "no-undo" "disp" "format" "with" "down" "frame"
		 "to" "param" "parameter" "entry" "put" "close" "run" "label" "no-box" "width"
		 "where" "no-lock" "skip" "column"
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
  (concat "[^\\.]\\<"
		  (regexp-opt ;;build me a regex worthy of mordor that matches all of these!
		   (append
			(mapcar 'upcase abl-keywords)
			'("{\.+}"))
		   t)
		  "\\>"))

(defconst abl-font-lock-keywords-1
  (list
   '(keyword-re . font-lock-builtin-face)))


;;Syntax
(defvar abl-mode-syntax-table
  (let ((st (make-syntax-table)))
	(modify-syntax-entry ?- "w" st) ;- and _ can be in words
	(modify-syntax-entry ?_ "w" st)
	(modify-syntax-entry ?/ ". 24" st)
	(modify-syntax-entry ?* ". 23" st)
	st))
						 




;;Indentation =================================================
;; Indentation Rule:
;;  0) Don't look at blank lines
;;  1) If we're at the beginning of the file, indent to 0.
;;  2) If the previous line ends in ":" (block defn) increase indentation.
;;  3) If **this** line is an "END." decrease indentation
;;  N) Otherwise stay const

(defun peek-prev (rexp)
  (save-excursion
	(progn
	  (forward-line -1)
	  (looking-at rexp))))

(defun prev-blank-p ()
  (peek-prev "^[ \\t]*$"))

(defun prev-dot-p ()
  (peek-prev ".*\\.[ \\t]*$"))

(defun prev-block-p ()
  (peek-prev ".*\\:[ \\t]*$"))

(defun indent-for-block ()
  (cond
   ((prev-blank-p)
	(+ (current-indentation) default-shift-width))
   ((prev-dot-p)
	(+ (current-indentation) default-shift-width))
   (t ; probably a multi-line block indent
	(current-indentation))))


(defmacro on-prev-line (&rest p)
  `(save-excursion
	   ,(append '(progn (forward-line -1)) p)))

;; (defun abl-indent-line ()
;;   (let (ind)
;; 	(cond
;; 	 ;;Rule 1
;; 	 ((bobp)
;; 	  (setq ind 0))
;; 	 ;; Don't count whitespace
;; 	 ((prev-blank-p)
;; 	  (on-prev-line
;; 	   (setq ind (abl-indent-line))))
;; 	 ;;Rule 2
;; 	 ((prev-block-p)
;; 	  (on-prev-line
;; 	   (setq ind (indent-for-block))))
;; 	 ;;Rule 3
;; 	 ((looking-at ".*END\\.[ \\t]$")
;; 	  (on-prev-line
;; 	   (setq (- (current-indentation) default-shift-width))))
;; 	 ;;Rule N
;; 	 (t
;; 	  (on-prev-line
;; 	   (setq ind (current-indentation)))))
;; 	(if ind
;; 		(if (> ind 0)
;; 			(indent-line-to ind)
;; 		  (indent-line-to 0)))
;; 	ind))





		  
	 
;; Auto-Capitalization
(define-abbrev-table 'abl-mode-abbrev-table
  (mapcar #'(lambda (v) (list v (upcase v) nil 1))
		  abl-keywords))

(abbrev-table-put abl-mode-abbrev-table :regexp (rx
												 (or line-start string-start (any " ("))
												 (group
												  (one-or-more (any "a-zA-Z0-9-_")))))


;; Misc
(defun misc ()
  (setq abbrev-mode t)
  (setq save-abbrevs nil)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq tab-stop-list (number-sequence 0 200 4))
  (linum-mode))



;; Synthesis

(define-derived-mode abl-mode
  fundamental-mode "ABL"
  "Major mode for editing ABL"
  (misc))

(provide 'abl-mode)


;; ===TODO:===
;;  * syntax highlighting!!!
;;  * better keyword syntax handling
;;  * indentation for assign statements and param lists (
;;  * comment syntax?
