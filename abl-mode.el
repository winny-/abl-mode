;;;; abl-mode.el --- Major mode for editing Progress OpenEdge/ABL

;; Copyright (C) 2015 Nathaniel Knight
;; Copyright (C) 2020 Winston Weinert

;; Author: Nathaniel Knight <nathaniel.ep@gmail.com>
;; Created: March 3rd, 2015
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; See the file license.txt for copying permissions.

;;;;  Commentary:
;; The mode provided by this file provides syntax highlighting and auto-
;; capitalization for the Progress Advanced Business Language (ABL), as
;; well as some basic navigation functions.

;;;; Code:

;;;; Initializations

(defvar abl-keyword-list
  '("def" "define" "as" "extent" "if" "then" "else" "end" "do" "display" "elseif"
    "endif" "message" "absolute" "and" "or" "assign" "available"
    "recid" "can-do" "can-find" "case" "when" "create" "day" "class"
    "month" "year" "procedure" "function" "forward"
    "returns" "temp-table" "for" "each" "delete" "in" "empty" "find"
    "handle" "first" "last" "length" "modulo" "not" "now" "today"
    "output" "stream" "index" "rindex" "replace" "round" "string"
    "rowid" "sqrt" "substring" "trim" "tran" "leave" "input" "output"
    "release" "return" "num-entries" "subst" "no-undo" "disp" "with"
    "down" "frame" "to" "param" "parameter" "entry" "put" "close"
    "run" "label" "no-box" "width" "where" "no-lock" "skip" "column"
    "unformatted" "by" "buffer" "group" "view-as" "alert-box" "field"
    "init" "initial" "query" "next" "on-error" "avail" "begins" "no-error"
    "retry" "undo" "error-status" "file-info" "input-output" "buffer-copy"
    "decimals" "table" "otherwise" "truncate" "ambiguous" "browse" "pause"
    "before-hide" "value" "export" "import" "from" "repeat" "like"
    "to-rowid" "substr" "unique" "primary" "overlay" "return-value"
    "on" "error" "min" "max" "buffer-value" "stop" "transaction" "is"
    "form" "stream-io" "column-label" "bind" "by-reference" "reference-only"
    "append" "using" "exclusive-lock" "chr" "terminal" "next-value"
    "current-value" "valid-handle" "valid-handle" "lastkey" "row"
    "buffer-field" "update" "upper" "lower" "throw" "catch" "centered"
    "help" "side-labels" "title" "prompt" "center" "scrolling" "hide" "clear"
    "all" "choose" "no-pause" "page-up" "page-down" "home" "go-on" "color"
    "cursor-left" "cursor-right" "cursor-up" "cursor-down" "normal"
    "next-prompt" "prev" "frame-line" "while" "except" "keyfunc" "random"
    "handle-type" "no-label" "page-number" "if-first" "if-last" "delimiter"
    "search" "time" "r-index" "etime" "persistent set" "table-handle" "of"
    "buffer-create" "default-buffer-handle" "finally" "prepare-temp-table"
    "lookup" "add-new-index" "add-new-field" "break by" "question" "yes-no"
    "yes-no-cancel" "buttons" "quoter" "query-prepare" "query-open" "set-buffers"
    "view" "object" "this-procedure" "persistent" "through" "set" "descending"
    "os-dir" "case-sensitive" "first-of" "last-of" "colon" "os-command" "silent"
    "var" "variable" "use-index"))

(defvar abl-type-list
  '("char" "character" "int" "integer" "format" "log" "logical"
    "date" "dec" "decimal" "datetime"))

(defvar abl-constant-list
  '("yes" "no" "true" "false"))

(defvar abl-mode-hook nil)

(defvar abl-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "<backtab>") 'abl-backtab)
	(define-key map (kbd "C-x n p") 'abl-narrow-to-proc)
	(define-key map (kbd "<M-up>") 'abl-backward-proc)
	(define-key map (kbd "<M-down>") 'abl-forward-proc)
	(define-key map (kbd "C-x v p") 'abl-occur-procs)
    map))




;;;; Highlighting
(defvar abl-keyword-regexp
  (regexp-opt abl-keyword-list 'words))

(defvar abl-string-regexp
  (rx (or (and "\""
                (zero-or-more (not (any "~\"")))
                "\"")
          (and "'"
               (zero-or-more (not (any "~'")))
                "'"))))

(defvar abl-type-regexp
  (regexp-opt abl-type-list 'words))

(defvar abl-constant-regexp
  (regexp-opt abl-constant-list 'words))

(defface abl-preprocessor-face
  '((t :inherit font-lock-preprocessor-face))
  "abl-mode preprocessor face")

(defvar abl-preprocessor-face 'abl-preprocessor-face)

(defvar abl-font-lock-keywords
  `(("&global-define\\|&scoped-define" . abl-preprocessor-face)
    ("\\({\\)\\([A-Za-z0-9-_.]+\\)[^{}]*\\(}\\)" (1 abl-preprocessor-face) (2 font-lock-string-face) (3 abl-preprocessor-face))
    ("\\({&\\)\\([A-Za-z0-9-_]+\\)\\(}\\)" (1 abl-preprocessor-face) (2 font-lock-string-face) (3 abl-preprocessor-face))
    ("\\_<define\\s-+[^.]*\\(?:browse\\|buffer\\|button\\|frame\\|image\\|menu\\|parameter\\|query\\|rectangle\\|stream\\|sub-menu\\|temp-table\\|work-table\\|workfile\\|variable\\)\\s-+\\(\\w+\\)\\|\\_<&global-define\\s-+\\(\\w+\\)\\|\\_<&scoped-define\\s-+\\(\\w+\\)" (1 font-lock-variable-name-face))
    ("^procedure\\s-+\\(\\w+\\)" (1 font-lock-function-name-face))
    ("^function\\s-+\\(\\w+\\)" (1 font-lock-function-name-face))
    (,abl-constant-regexp . font-lock-constant-face)
    (,abl-keyword-regexp . font-lock-builtin-face)
    (,abl-type-regexp . font-lock-type-face)))

(defvar abl-font-lock-defaults
  `(,abl-font-lock-keywords nil t))

;;;; Syntax

(defvar abl-syntax-table
  (let ((st (make-syntax-table)))
	(modify-syntax-entry ?- "w" st) ;- and _ can be in words
	(modify-syntax-entry ?_ "w" st)
	(modify-syntax-entry ?/ ". 14n" st)
	(modify-syntax-entry ?* ". 23n" st)
	(modify-syntax-entry ?~ "\\" st)
        (modify-syntax-entry ?' "\"" st)
	(modify-syntax-entry ?= "w" st) ; = can be a word (For navigation)
	st))
						 

;;;; Auto-Capitalization
(defvar abl-abbrev-word-regexp  ;; regexp which triggers abbrev expansion
  (rx
   (or line-start string-start (any " (:"))
   (group
	(one-or-more (any "a-zA-Z0-9-_"))
	(zero-or-more (any "-_")))))

(define-abbrev-table 'abl-mode-abbrev-table
  (mapcar #'(lambda (v) (list v (upcase v) nil 1))
		  (append abl-keyword-list abl-type-list)))

;; (abbrev-table-put abl-mode-abbrev-table
;; 				  :regexp abl-abbrev-word-regexp)

(defun abl-pre-abbrev-expand-hook ()
  (setq local-abbrev-table
		(if (abl-in-code-context-p)
			abl-mode-abbrev-table)))


;; Code Context callback
(defun abl-in-comment-p ()
  "Return t if point is not in a comment or a string."
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
  

;;;; Text Modification

(defun abl-assign-insert-tablename (tbl)
  "At the beginning of an assign statmenet, add a table or temp-table name to 
the beginning of each of the variables being assigned."
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


;;;; Reading & Navigation Code
(defun abl-narrow-to-proc ()
  "When inside a function or internal procedure definition, narrow the buffer 
to that definition."
  (interactive)
  (let (b e)
	(save-excursion
	  (search-backward-regexp (rx line-start (or "FUNCTION" "PROCEDURE")))
	  (setq b (point))
	  (search-forward-regexp (rx line-start "END"))
	  (end-of-line)
	  (setq e (point)))
	(narrow-to-region b e)))

(defun abl-occur-procs ()
  "Show occurences of FUNCTION and PROC."
  (interactive)
  (occur (rx line-start (or "FUNCTION" "PROC"))))

(defun abl-forward-proc ()
  "Move the point forward to the beginning of the next function or internal procedure
definition."
  (interactive)
  (right-char)
  (search-forward-regexp (rx line-start (or "FUNCTION" "PROCEDURE")))
  (beginning-of-line))

(defun abl-backward-proc ()
    "Move the point backwards to the beginning of the previous function or internal procedure
definition."
  (interactive)
  (search-backward-regexp (rx line-start (or "FUNCTION" "PROCEDURE")))
  (beginning-of-line))

(defun abl-backtab ()
  "Decrease the indentation of the current line by 4 spaces."
  (interactive)
  (save-excursion
	(beginning-of-line)
	(when (looking-at "    ")
	  (replace-match ""))))


;;;; Skeletons

(define-skeleton abl-skel-test
  "Insert the beginning of a test procedure."
  nil
  "PROCEDURE test_" (skeleton-read "Name:") ":" \n
  "    DEF OUTPUT PARAM p_pass     AS LOG  NO-UNDO." \n
  "DEF OUTPUT PARAM p_msg      AS CHAR NO-UNDO." \n
  - \n "END.\n")


;; Synthesis =========================================================
(define-derived-mode abl-mode
  prog-mode "ABL"
  "Major mode for editing ABL"
  :syntax-table abl-syntax-table
  (setq-local font-lock-keywords-case-fold-search t)
  (set (make-local-variable 'font-lock-defaults) abl-font-lock-defaults)
  (use-local-map abl-mode-map)
  ;; (progn
  ;;       (make-local-variable 'pre-abbrev-expand-hook)
  ;;       (add-hook 'pre-abbrev-expand-hook 'abl-pre-abbrev-expand-hook)
  ;;       (abbrev-mode 1))  
  ;; (set (make-local-variable  'abbrev-mode) t)
  (set (make-local-variable  'save-abbrevs) nil)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'tab-width) 4)
  (set (make-local-variable 'tab-stop-list) (number-sequence 0 200 4)))

(add-to-list 'auto-mode-alist '("\\.p\\'" . abl-mode))
(provide 'abl-mode)
