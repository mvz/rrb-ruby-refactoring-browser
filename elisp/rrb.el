;;; rrb.el -- Ruby Refactoring Brower emacs interface

;; Copyright (C) 2003 OHBAYASHI Ippei, YOSHIDA Yuichi, HARA Yutaka

;; Author: OHBAYASHI Ippei <ohai@kmc.gr.jp>
;;	YOSHIDA Yuichi <oxy@kmc.gr.jp>
;;	HARA Yutaka <yhara@kmc.gr.jp>
;; Keywords: ruby refactoring browser

;;; Code:

;;;; Customizable variables
(defvar rrb-ruby-file-name-regexp "^.*\\.rb$"
  "*Regular expression matched ruby script file name")

(defvar rrb-tmp-file-base "rrblog"
  "*Base file name to use error log file")

(defvar rrb-undo-file-base "rrbundo"
  "*Base file name for undo file")
(defvar rrb-undo-file
  (expand-file-name "rrbundo" temporary-file-directory))


;;;; Internal variables
(defconst rrb-io-splitter "\C-a")
(defconst rrb-io-terminator "-- END --")

(defvar rrb-input-buffer (get-buffer-create " *rrb-input*"))
(defvar rrb-output-buffer (get-buffer-create " *rrb-output*"))
(defvar rrb-error-buffer (get-buffer-create " *rrb-error*"))
(defvar rrb-default-value-buffer (get-buffer-create " *rrb-default-value*"))

(defvar rrb-undo-count 0)

(add-hook 'kill-emacs-hook 'rrb-delete-undo-files)

;;;; Utility functions
(defun rrb-find-all (prec list)
  "Return list containing all elements of PREC is true"
  (cond ((eq list nil) nil)
	((funcall prec (car list)) (cons (car list)
					 (rrb-find-all prec (cdr list))))
	(t (rrb-find-all prec (cdr list)))))

(defun rrb-buffer-map-line (prec)
  "Return list of (PREC line) for each line in current buffer"
  (mapcar prec
	  (split-string (buffer-string) "\n")))

(defun rrb-chop (str)
  "Return chopped string"
  (substring str 0 -1))

(defun rrb-current-line ()
  "Return the vertical position of point..."
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0)))
     
;;;; Main functions

(defun rrb-all-ruby-script-buffer ()
  "Return all ruby script buffer,`ruby script buffer' means `its file name
matches with rrb-ruby-file-name-regexp'"
  (rrb-find-all (lambda (buffer)
		  (and (buffer-file-name buffer) 
		       (string-match rrb-ruby-file-name-regexp
				     (buffer-file-name buffer))))
		(buffer-list)))

(defun rrb-insert-input-string (src-buffer)
  (insert (buffer-file-name src-buffer))
  (insert rrb-io-splitter)
  (insert-buffer-substring src-buffer)
  (insert rrb-io-splitter))

(defun rrb-setup-buffer (buffer buffer-list)
  "Generate input string on \" *rrb-input*\""
  (save-current-buffer
    (set-buffer buffer)
    (erase-buffer)
    (mapcar 'rrb-insert-input-string
	    buffer-list)
    (insert rrb-io-terminator)
    (insert rrb-io-splitter)))

(defun rrb-clean-output-buffer ()
  "Clean temporary buffers"
  (save-current-buffer
    (set-buffer rrb-output-buffer)
    (erase-buffer)
    (set-buffer rrb-error-buffer)
    (erase-buffer)))

(defun rrb-output-to-buffer-and-reset-point (alist)
  "Rewrite all ruby script buffer from \" *rrb-output\" and reset cursor point"
  (save-current-buffer
    (set-buffer rrb-output-buffer)
    (let ((list-top (split-string (buffer-string) rrb-io-splitter)))
      (while (not (string= (car list-top) rrb-io-terminator))
	(set-buffer (get-file-buffer (car list-top)))
	(erase-buffer)
	(insert (cadr list-top))
	(goto-char (cdr (assq (current-buffer) alist)))
	(setq list-top (cddr list-top))))))

(defun rrb-output-to-error-buffer (filename)
  "load FILENAME to \" *rrb-error\""
  (save-current-buffer
    (set-buffer rrb-error-buffer)
    (insert-file-contents filename)))

(defun rrb-error-message ()
  "Get Error Message in \" *rrb-error\""
  (save-current-buffer
    (set-buffer rrb-error-buffer)
    (goto-char (point-min))
    (substring (thing-at-point 'line) 0 -1)))
    
(defun rrb-do-refactoring (&rest args)
  "Do refactoring"
  (let ((buffer-point-alist (rrb-buffer-point-alist)))
    (if (/= (apply 'rrb-run-process "rrb" args) 0)
	(error "fail to refactor: %s" (rrb-error-message)))
    (setq rrb-undo-count (+ rrb-undo-count 1))
    (rrb-output-to-buffer-and-reset-point buffer-point-alist)))

(defun rrb-make-temp-name (base)
  (make-temp-name (expand-file-name base temporary-file-directory)))

(defun rrb-run-process (command &rest args)
  "Run COMMAND and return error code"
  (save-current-buffer
    (let ((error-code)
	  (tmpfile (rrb-make-temp-name rrb-tmp-file-base)))
      (rrb-clean-output-buffer)
      (set-buffer rrb-input-buffer)
      (setq error-code (apply 'call-process-region
			      (point-min) (point-max)
			      command
			      nil
			      (list rrb-output-buffer tmpfile)
			      nil
			      `(,@args "--stdin-stdout")))
      (rrb-output-to-error-buffer tmpfile)
      (delete-file tmpfile)
      error-code)))

(defun rrb-buffer-point-alist ()
  (save-current-buffer
    (mapcar (lambda (buf)
	      (set-buffer buf)
	      (cons buf (point)))
	    (rrb-all-ruby-script-buffer))))

(defun rrb-prepare-refactoring ()
  (save-current-buffer
    (rrb-setup-buffer rrb-input-buffer (rrb-all-ruby-script-buffer))
    (set-buffer rrb-input-buffer)
    (write-region (point-min) (point-max) (format "%s%d" rrb-undo-file rrb-undo-count) nil 0 nil)))

(defun rrb-delete-undo-files ()
  (let ((sub-files 
	 (directory-files temporary-file-directory t rrb-undo-file-base)))
    (while (not (null sub-files))
      (let ((sub-file (car sub-files)))
	(delete-file sub-file))
      (setq sub-files (cdr sub-files)))))


;;;; Completion

;;;
;;; completion type-1 ( str ; str,str,...,str)
;;
;; rrb_compinfo output example
;;
;;  A#x;heke,uhe,hoge
;;  A::B#y;hoge
;;  a::B#z;i,j,k,l
;;
(defun rrb-complist-first-for-type-1 ()
  (save-current-buffer
    (set-buffer rrb-output-buffer)
    (rrb-buffer-map-line (lambda (line) (split-string line ";")))))

(defun rrb-complist-second-for-type-1 (key)
  (save-current-buffer
    (set-buffer rrb-output-buffer)
    (goto-char (point-min))
    (when (search-forward (concat key ";") nil t)
      (mapcar 'list
	      (split-string (buffer-substring (point) (point-at-eol)) ",")))))

(defun rrb-comp-read-type-1 (compinfo-arg default-arg prompt1 prompt2 prompt3)
  "completion read for Rename local variable, Rename instance variable, etc.."
  (when (/= (rrb-run-process "rrb_compinfo" compinfo-arg) 0)
    (error "rrb_info: fail to get information %s" (rrb-error-message)))
  (let ((method (completing-read prompt1
				 (rrb-complist-first-for-type-1) nil nil default-arg)))
    (list method
 	  (completing-read prompt2 (rrb-complist-second-for-type-1 method))
 	  (read-from-minibuffer prompt3))))

;;;
;;; completion type-2  ( str,str,...,str, )
;;
;; rrb_compinfo output example
;;
;;  A,A::B,A::C,A::C::C1,A::C::C2,
;;
(defun rrb-complist-type-2 ()
  (save-current-buffer
    (set-buffer rrb-output-buffer)
    (goto-char (point-min))
    (mapcar 'list
            (split-string (buffer-substring (point-at-bol) (point-at-eol)) ","))))

(defun rrb-comp-read-type-2 (compinfo-arg default-arg prompt1 prompt2)
  "Completion read for Rename method all, Rename Constant, etc.."
  (when (/= 0 (rrb-run-process "rrb_compinfo" compinfo-arg))
    (error "rrb_info: fail to get information %s" (rrb-error-message)))
  (list (completing-read prompt1 (rrb-complist-type-2) nil nil default-arg)
	(read-from-minibuffer prompt2)))

;;;
;;; completion type-3
;;
(defun rrb-comp-read-type-3 (compinfo-arg1 default-arg1 prompt1 compinfo-arg2 prompt2)
  "completion read for Pull up method, etc.."
  (when (/= (rrb-run-process "rrb_compinfo" compinfo-arg1) 0)
    (error "rrb_info: fail to get information %s" (rrb-error-message)))
  (let ((old-class-method (completing-read prompt1 (rrb-complist-type-2) nil nil default-arg1)))
    (when (/= (rrb-run-process "rrb_compinfo" compinfo-arg2) 0)
      (error "rrb_info: fail to get information %s" (rrb-error-message)))
    (list old-class-method
	  (completing-read prompt2 (rrb-complist-type-2)))))

;;;
;;; completion type-4
(defun rrb-comp-read-type-4 (compinfo-arg1 default-arg1 prompt1 compinfo-arg2 prompt2 prompt3)
  "completion read for rename instance variable, etc.."
  (when (/= (rrb-run-process "rrb_compinfo" compinfo-arg1) 0)
    (error "rrb_info: fail to get information %s" (rrb-error-message)))
  (let ((retval-1 (completing-read prompt1 (rrb-complist-type-2) nil nil default-arg1)))
    (when (/= (rrb-run-process "rrb_compinfo" compinfo-arg2 "--target" retval-1) 0)
      (error "rrb_info: fail to get information %s" (rrb-error-message)))
    (list retval-1
	  (completing-read prompt2 (rrb-complist-type-2))
	  (read-from-minibuffer prompt3))))

;;;
;;; completionm type-5
(defun rrb-comp-read-type-5 (compinfo-arg1 default-arg1 prompt1 compinfo-arg2 prompt2 prompt3)
  "completion read for rename method, etc.."
  (when (/= (rrb-run-process "rrb_compinfo" compinfo-arg1) 0)
    (error "rrb_info: fail to get information %s" (rrb-error-message)))
  (let ((retval-1 "")
	(result)
	(looped nil))
    (while (progn 
	     (setq result (completing-read prompt1 (rrb-complist-type-2) nil nil (if looped "" default-arg1)))
	     (not (string= result "")))
      (setq looped t)
      (setq retval-1 (format "%s %s" retval-1 result))
      (message (format "%s %s" "inputed data: " retval-1))
      (sit-for 1))
    (when (/= (rrb-run-process "rrb_compinfo" compinfo-arg2 "--target" retval-1) 0)
      (error "rrb_info: fail to get information %s" (rrb-error-message)))
    (list retval-1
	  (completing-read prompt2 (rrb-complist-type-2))
	  (read-from-minibuffer prompt3))))

;;;
;;; completionm type-6
(defun rrb-comp-read-type-6 (compinfo-arg1 default-arg1 prompt1 prompt2 prompt3)
  "completion read for rename method, etc.."
  (when (/= (rrb-run-process "rrb_compinfo" compinfo-arg1) 0)
    (error "rrb_info: fail to get information %s" (rrb-error-message)))
  (let ((namespace (completing-read prompt1 (rrb-complist-type-2) nil nil default-arg1))
	(new-class (read-from-minibuffer prompt2)))
    (let ((targets "")
	  (result))
      (while (progn 
	       (setq result (read-from-minibuffer prompt3))
	       (not (string= result "")))
	(setq targets (format "%s %s" targets result))
	(message (format "%s %s" "inputed data: " targets))
	(sit-for 1))
      (list namespace new-class targets))))



;;;
;;; default value
;;    
(defun rrb-get-value-on-cursor (args)
  (save-current-buffer
    (rrb-setup-buffer rrb-default-value-buffer (list (current-buffer)))
    (if (/= (rrb-run-process "rrb_default_value" (buffer-file-name) (number-to-string (rrb-current-line)) args) 0)
	""
      (set-buffer rrb-output-buffer)
      (buffer-substring (point-min) (point-max)))))

;;;
;;; Undo, Redo
;;;
(defun rrb-undo-base (count-func)
  "Base of Undo and Redo of the last refactoring"
  (let ((undo-file-name 
	 (format "%s%d" rrb-undo-file (funcall count-func rrb-undo-count))))
    (if (file-readable-p undo-file-name)
	(progn 
	  (rrb-prepare-refactoring)
	  (setq rrb-undo-count (funcall count-func rrb-undo-count))
	  (save-current-buffer
	    (rrb-clean-output-buffer)
	    (set-buffer rrb-output-buffer)
	    (insert-file-contents undo-file-name)
	    (rrb-output-to-buffer-and-reset-point (rrb-buffer-point-alist)))))))
;;;
;;; Undo
;;
(defun rrb-undo ()
  "Undo of the last refactoring"
  (interactive)
  (rrb-undo-base (lambda (count) (- count 1))))

;;;
;;; Redo
;;
(defun rrb-redo ()
  "Redo of the last Undo"
  (interactive)
  (rrb-undo-base (lambda (count) (+ count 1))))

	  

;;;; Refactoring: Rename local variable 
(defun rrb-comp-read-rename-local-variable ()
  "Completion read for Rename local variable"
  (rrb-comp-read-type-4 "--methods" (rrb-get-value-on-cursor "--method") "Refactored method: " "--local-vars" "Old variable: " "New variable: "))

(defun rrb-rename-local-variable (method old-var new-var)
  "Refactor code: rename local variable"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-rename-local-variable)))
  (save-current-buffer
    (rrb-do-refactoring "--rename-local-variable" method old-var new-var)))

;;;; Refactoring: Rename method
(defun rrb-comp-read-rename-method ()
  "Completion read for Rename method"
  (rrb-comp-read-type-5 "--classes" 
			(rrb-get-value-on-cursor "--class")
			"Refactored classes(type just RET to finish): "
			"--bare-methods"
			"Old method: "
			"New method: "))

(defun rrb-rename-method (classes old-method new-method)
  "Refactor code: rename local variable"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-rename-method)))
  (save-current-buffer
    (rrb-do-refactoring "--rename-method" classes old-method new-method)))

;;;; Refactoring: Rename method all
(defun rrb-comp-read-rename-method-all ()
  "Completion read for Rename method all"
  (rrb-comp-read-type-2 "--bare-methods" 
			(rrb-get-value-on-cursor "--bare-name")
			"Old method: " "New method: "))

(defun rrb-rename-method-all (old-method new-method)
  "Refactor code: rename method all old method as new"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-rename-method-all)))
  (save-current-buffer
    (rrb-do-refactoring "--rename-method-all" old-method new-method)))

;;;; Refactoring: Extract method
(defun rrb-begin-line-num (begin)
  (goto-char begin)
  (if (eolp)
      (forward-char)
    (beginning-of-line))
  (1+ (count-lines (point-min) (point))))

(defun rrb-end-line-num (end)
  (goto-char end)
  (if (bolp)
      (backward-char))
  (count-lines (point-min) (point)))

(defun rrb-extract-method (begin end new_method)
  "Refactor code: Extract method"
  (interactive "r\nsNew method: ")
  (save-current-buffer
    (rrb-prepare-refactoring)
    (rrb-do-refactoring "--extract-method"
			(buffer-file-name)
			new_method
			(number-to-string (rrb-begin-line-num begin))
			(number-to-string (rrb-end-line-num end)))))

;;;; Refactoring: Rename instance variable
(defun rrb-comp-read-rename-instance-variable ()
  "completion read for rename instance variable"
  (rrb-comp-read-type-4 "--classes" 
			(rrb-get-value-on-cursor "--class") 
			"Refactord class: " 
			"--instance-vars"
			"Old instance variable: "
			"New instance variable: " ))

(defun rrb-rename-instance-variable (ns old-var new-var)
  "Refactor code: Rename instance variable"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-rename-instance-variable)))
  (save-current-buffer
    (rrb-do-refactoring "--rename-instance-variable" ns old-var new-var)))

;;;; Refactoring: Rename class variable
(defun rrb-comp-read-rename-class-variable ()
  "completion read for rename instance variable"
  (rrb-comp-read-type-4 "--classes"
			(rrb-get-value-on-cursor "--class") 
			"Refactord class: " 
			"--class-vars"
			"Old class variable: " 
			"New class variable: " ))

(defun rrb-rename-class-variable (ns old-var new-var)
  "Refactor code: Rename instance variable"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-rename-class-variable)))
  (save-current-buffer
    (rrb-do-refactoring "--rename-class-variable" ns old-var new-var)))


;;;; Refactoring: Rename global variable
(defun rrb-comp-read-rename-global-variable ()
  "compleion read for rename global variable"
  (rrb-comp-read-type-2 "--global-vars"
			""
			"Old global variable: " "New global variable: "))

(defun rrb-rename-global-variable (old-var new-var)
  "Refactor code: Rename global variable"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-rename-global-variable)))
  (save-current-buffer
    (rrb-do-refactoring "--rename-global-variable" old-var new-var)))

;;;; Refactoring: Rename constant
(defun rrb-comp-read-rename-constant ()
  "compleion read for rename constant"
  (rrb-comp-read-type-2 "--constants" 
			(rrb-get-value-on-cursor "--class")
			"Old constant: " "New constant: "))

(defun rrb-rename-constant (old-const new-const)
  "Refactor code: Rename constant"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-rename-constant)))
  (save-current-buffer
    (rrb-do-refactoring "--rename-constant" old-const new-const)))

;;;; Refactoring: Rename class

(defun rrb-comp-read-rename-class ()
  "compleion read for rename class"
  (rrb-comp-read-type-2 "--classes" 
			(rrb-get-value-on-cursor "--class")
			"Old class/module: " "New class/module: "))

(defun rrb-rename-class (old-class new-class)
  "Refactor code: Rename class or module"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-rename-class)))
  (save-current-buffer
    (rrb-do-refactoring "--rename-constant" old-class new-class)))
			;class name is a constant.

	       
;;;; Refactoring: Pull up method

(defun rrb-comp-read-pullup-method ()
  "completion read for pull up method"
  (rrb-comp-read-type-3 "--methods"
			(rrb-get-value-on-cursor "--method")
			"Old Method: " "--classes" "New class: "))

(defun rrb-pullup-method (old-method new-class)
  "Refactor code: Pull up method"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-pullup-method)))
  (save-current-buffer
    (rrb-do-refactoring "--pullup-method" old-method new-class
			(buffer-file-name)
			(number-to-string (rrb-current-line)))))
		 
;;;; Refactoring: Push down method

(defun rrb-comp-read-pushdown-method ()
  "completion read for push down method"
  (rrb-comp-read-type-3 "--methods"
			(rrb-get-value-on-cursor "--method")
			"Old Method: " "--classes" "New class: "))

(defun rrb-pushdown-method (old-method new-class)
  "Refactor code: Push down method"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-pushdown-method)))
  (save-current-buffer
    (rrb-do-refactoring "--pushdown-method" old-method new-class 
			(buffer-file-name)
			(number-to-string (rrb-current-line)))))
		 
;;;; Refactoring: Extract superclass
(defun rrb-comp-read-extract-superclass ()
  "completion read for extract superclass"
  (rrb-comp-read-type-6 "--classes"
		       (rrb-get-value-on-cursor "--class")
		       "Location: " "New Class: " 
		       "Targets(type just RET to finish): "))

(defun rrb-extract-superclass (namespace new-class targets)
  "Refactor code: Extract superclass"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-extract-superclass)))
  (save-current-buffer
    (rrb-do-refactoring "--extract-superclass" namespace new-class targets
			(buffer-file-name)
			(number-to-string (rrb-current-line)))))