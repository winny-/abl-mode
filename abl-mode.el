;; ABL-Mode
;;
;; An emacs major mode for editing Progress/ABL files.
;;
;; Copyright Nat Egan-Pimblett 2015
;; nathaniel.ep@gmail.com
;;
;; Provided under the MIT License

(define-derived-mode abl-mode
  prog-mode "ABL"
  "Major mode for editing ABL"
  (setq font-lock-defaults '(abl-font-lock-defaults))
;  (setq indent-line-function 'abl-indent-line)  ;this isn't ready
  (use-local-map abl-mode-map)
  (setq abbrev-mode t)
  (set-syntax-table abl-syntax-table)
  (setq save-abbrevs nil)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq tab-stop-list (number-sequence 0 200 4))
  (setq comment-start "/*")
  (setq comment-end "*/"))




;; Kewords ============================================================
(defvar abl-keyword-list
 '("def" "define" "as" "extent" "if" "then" "else" "end" "do" "elseif"
	"endif" "message" "absolute" "and" "or" "assign" "available"
	"beings" "recid" "can-do" "can-find" "case" "when" "create" "day"
	"month" "year" "datetime" "procedure" "function" "forward"
	"returns" "temp-table" "for" "each" "delete" "in" "empty" "find"
	"handle" "first" "last" "length" "modulo" "not" "now" "today"
	"output" "stream" "index" "rindex" "replace" "round" "string"
	"rowid" "sqrt" "substring" "trim" "tran" "leave" "input" "output"
	"release" "return" "num-entries" "subst" "no-undo" "disp" "with"
	"down" "frame" "to" "param" "parameter" "entry" "put" "close"
	"run" "label" "no-box" "width" "where" "no-lock" "skip" "column"
	"unformatted" "by" "buffer" "group" "view-as" "alert-box" "field"
	"init" "query" "next" "no-error" "avail" "begins" "no-error"
	"retry" "undo" "error-status" "file-info" "input-output" ))


(defvar abl-type-list
  '("char" "character" "int" "integer" "format" "var" "variable" "log" "logical"
	"yes" "no" "true" "false" "date" "dec" "decimal"))

;; Init ============================================================
(defvar abl-mode-hook nil)

(defvar abl-mode-map
  (let ((map (make-keymap)))
	(define-key map "<backtab>" 'abl-backtab)
    ;;Define mode-specific keybindings here
    map))

(add-to-list 'auto-mode-alist '("\\.p\\'" . abl-mode))


;;Highlighting ==================================================
;;TODO: vars? types?
(defvar abl-keyword-regexp
  (regexp-opt (mapcar 'upcase (append abl-keyword-list abl-type-list)) 'words))

(defvar abl-string-regexp
  (rx (and "\""
		   (zero-or-more
			(or (not (any "~\""))
				"\n"))
		   "\""))
  "Regexp which matches a string")

(defvar abl-comment-regexp
  "\\(/\\*.*\\*/\\)"
  "Regexp which matches an ABL comment")

(defvar abl-font-lock-defaults
  `((,abl-keyword-regexp . (1 font-lock-builtin-face))
;	(,abl-comment-regexp . (1 font-lock-comment-face))
	(,abl-string-regexp . (1 font-lock-string-face))))

;;Syntax====================================
;;TODO comment syntax
(defvar abl-syntax-table
  (let ((st (make-syntax-table)))
	(modify-syntax-entry ?- "w" st) ;- and _ can be in words
	(modify-syntax-entry ?_ "w" st)
	(modify-syntax-entry ?/ ". 14" st)
	(modify-syntax-entry ?* ". 23" st)
	(modify-syntax-entry ?~ "\\" st)
	st))
						 

;; Auto-Capitalization ======================
(define-abbrev-table 'abl-mode-abbrev-table
  (mapcar #'(lambda (v) (list v (upcase v) nil 1))
		  (append abl-keyword-list abl-type-list)))

(defun abl-in-comment-p ()
  (let ((open (save-excursion
				(search-backward "/*" 0 t)))
		(close (save-excursion
				 (search-backward "*/" 0 t))))
	(cond
	 ((null open)
	  nil)
	 ((and (null close) open)
	  t)
	 (t
	  (> open close)))))

(defun abl-in-string-p ()
  (let* ((beg (save-excursion
				(beginning-of-line)
				(point)))
		 (l (parse-partial-sexp beg (point))))
	(nth 3 l)))

(defun abl-in-code-context-p ()
  (and (not (abl-in-comment-p))
	   (not (abl-in-string-p))))
  

(defvar abl-abbrev-word-regexp
  (rx
   (or line-start string-start (any " (:"))
   (group
	(one-or-more (any "a-zA-Z0-9-_"))
	(zero-or-more (any "-_")))))



(abbrev-table-put abl-mode-abbrev-table
				  :regexp abl-abbrev-word-regexp)






;;Indentation =================================================
;; Indentation Rule:
;; * first line is 0
;; * don't look at blank lines
;; * lines ending in : indent
;; * end. lines unindent


;;-- helpers
(defmacro on-prev-line (&rest p)
  `(save-excursion
	   ,(append '(progn (forward-line -1)) p)))

(defun seek-prev (rexp)
  (save-excursion
    (forward-line -1)
    (cond
     ((bobp) nil)
     ((looking-at "^[ \t]*$")
      (seek-prev rexp))
     (t
      (looking-at rexp)))))

(defun seek-pprev (rexp)
  (save-excursion
    (cond
     ((bobp) nil)
     ((looking-at "^[ \t]*$")
      (seek-prev rexp))
     (t
      (looking-at rexp)))))

(defun prev-line-indentation ()
  (save-excursion
    (search-backward-regexp (rx
                             (not (any whitespace))))
    (current-indentation)))

;;-- diagnostics
(defun prev-blank-p ()
  (seek-prev "^[ \\t]*$"))

(defun prev-ends-blankish-p ()
  (not (seek-prev ".*[\\.,][ \t]*$")))

(defun prev-ends-dottish-p ()
  (seek-prev ".*\\.[ \t]*$"))



(defun prev-begins-block-p ()
  (let ((begin-block-regexp ".*[ \t]*:[ \t]*$"))
    (seek-prev begin-block-regexp)))

(defun line-ends-block-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at " *END[\t *]*\\.[\t *]*$")))

;;-- handlers
(defun get-ind-after-blankish-ending ()
  (interactive)
  (save-excursion
    (forward-line -1)
    (cond
     ((or (prev-ends-dottish-p) (prev-begins-block-p))
      (if (on-prev-line (looking-at "^[ \t]*("))
          ;; special treatment for multi-line paren statemetns
          (+ (current-indentation) tab-width 1) 
        (+ (current-indentation) tab-width)))
     (t
      (current-indentation)))))

(defun get-ind-after-dottish-ending ()
  (interactive)
  (save-excursion
    (let (done ind)
      (while (not done)
        (forward-line -1)
        (cond
         ((looking-at ""))
         ;;Previous normal statement or end of a (,) group
         ((or (prev-ends-dottish-p) (prev-begins-block-p)) 
          (current-indentation))
         ;;
         (t
          (- (current-indentation) tab-width)))))))

(defun get-ind-after-begin-block ()
  (save-excursion
    (search-backward-regexp ":[ \t]*$")
    (if (prev-ends-dottish-p)
        (+ (current-indentation) tab-width)
      (progn
        (let (done ind)
          (while (not done)
            (forward-line -1)
            (cond
             ((bobp)
              (setq done t)
              (setq ind tab-width))
             ((prev-ends-dottish-p)
              (setq done t)
              (setq ind (+ (current-indentation) tab-width)))))
          ind)))))


;;-- synthesis
(defun abl-indent-line ()
  ;; Decide what indentation should be
  (let (ind)
    (cond
     ;;Beginning of buffer starts un-indented
     ((bobp)
      (setq ind 0))
     ;;END. statements unindent the next line
     ((line-ends-block-p)
      (setq ind (- (prev-line-indentation) tab-width)))
     ;;Ending in ":" indents the next line
     ((prev-begins-block-p )
      (setq ind (get-ind-after-begin-block)))
     ;;Ignore blank lines
     ((prev-blank-p)
      (setq ind (on-prev-line (abl-indent-line))))
     ((prev-ends-blankish-p)
      (setq ind (get-ind-after-blankish-ending)))
     ((prev-ends-dottish-p)
      (setq ind (get-ind-after-dottish-ending)))
     ;;Otherwise, just guess that it's the same as the previous
     (t
      (setq ind (prev-line-indentation))))
    ;; Set indentation
    (if (> ind 0)
        (indent-line-to ind)
      (indent-line-to 0))
    ;; And, so that we can call recursively, return
    ind))





;; Synthesis


(provide 'abl-mode)


;; ===TODO:===
;;  * syntax highlighting!!!
;;  * better keyword syntax handling
;;  * indentation for assign statements and param lists (
;;  * comment syntax?


;; Functions ================================================================

(defun abl-assign-insert-tablename (tbl)
  (interactive "sTable name: ")
  (let (done)
	(while (not done)
	  (if (looking-at "^[ \t]*$")
		  (forward-line 1)
		(progn
		  (beginning-of-line)
		  (forward-word)
		  (backward-word)
		  (insert tbl ".")
		  (if (looking-at ".*\\.[\t ]*$")
			  (setq done t)
			(forward-line 1)))))))

(defun abl-backtab ()
  (save-excursion
	(beginning-of-line)
	(when (looking-at "    ")
	  (replace-match))))

